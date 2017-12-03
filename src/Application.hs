module Application (
    runApplication
) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Data.IORef
import Data.Maybe
import qualified Data.Set as Set
import Data.Tree
import Data.Tree.Zipper
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (get, ($=),($~),($~!)) -- could also be used with IORef
import qualified Graphics.UI.GLFW as GLFW
import Numeric (showGFloat)
import System.Log.Logger

import FunctionalGL
import Layouts
import Scene
import View
import World

----------------------------------------------------------------------------------------------------

type SceneUI = UI (Maybe Scene)

createSceneView :: Bool -> Layout (Maybe Scene) -> ViewHandleEvent (Maybe Scene) -> Maybe Scene -> View (Maybe Scene)
createSceneView hasFocus layout handleEvent content = (createView content)
    { viewHandleEvent = handleEvent
    , viewLayout = layout
    , viewHasFocus = hasFocus
    }

{- | Handle an event through simple delegation to its inner content scene, if any. A scene being not
aware of the view holding it, any change will stay local (excepted for a Nothing interpreted as a
closure).
-}
sceneHandleEvent :: IORef ResourceMap -> ViewHandleEvent (Maybe Scene)
sceneHandleEvent currentResources event treeLoc =
    let view = getLabel treeLoc
        (_, size) = viewLocalBounds view
    in case viewContent view of
        Nothing -> return BubbleUp
        Just scene -> do
            resources <- GL.get currentResources
            (content', resources') <- runStateT (sceneManipulate scene size event) resources
            currentResources $= resources'
            case content' of
                Nothing -> return Terminate
                Just scene' ->
                    let view' = view{ viewContent = Just scene' }
                    in  return (Consume (modifyLabel (const view') treeLoc))

----------------------------------------------------------------------------------------------------

runApplication :: String -> IO ()
runApplication name = do
    let (w , h) = (800, 600)
        size = GL.Size (fromIntegral w) (fromIntegral h)

    (GLFW.Version x y z) <- GLFW.getVersion
    infoM "Kage" ("Starting GLFW " ++ show x ++ "." ++ show y ++ "." ++ show z)

    GLFW.setErrorCallback (Just (\e s -> errorM "Kage" (show e ++ ": " ++ show s)))

    True <- GLFW.init
    mapM_ GLFW.windowHint
        [   GLFW.WindowHint'Samples 4 -- 4x antialiasing (not very useful in deferred shading)
        ,   GLFW.WindowHint'ContextVersionMajor 4 --
        ,   GLFW.WindowHint'ContextVersionMinor 3 -- OpenGL >= 4.3 (required for compute shaders)
        ,   GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
        ]
    (Just window) <- GLFW.createWindow w h name Nothing Nothing
    GLFW.makeContextCurrent (Just window)
    GLFW.setWindowSize window w h -- must happen *after* 'createWindow'

    mapM_ (\(k, v) -> GL.get v >>= infoM "Kage" . ((k ++ ": ") ++))
        [   ("Renderer", GL.renderer)
        ,   ("OpenGL version", GL.glVersion)
        ,   ("Shading language version", GL.shadingLanguageVersion)
        ]

    let resources0 = newResourceMap

    ((currentWorld, currentContext), resources1) <- flip runStateT resources0 $ do
        currentWorld <- liftIO . newIORef =<< createWorld
        currentContext <- liftIO . newIORef =<< createContext
        return (currentWorld, currentContext)

    currentResources <- newIORef resources1

    let masterScene = createScene DeferredShading currentWorld currentContext
        radarScene = createScene ForwardShading currentWorld currentContext
        [shadowScene, positionScene, normalScene, albedoAndSpecularScene] =
            map (`createColorBufferScene` currentContext) [0..3]

    let handleEvent = sceneHandleEvent currentResources
    uiRef <- newIORef . createUI $
        Node (createSceneView False adaptativeLayout handleEvent Nothing)
        [   Node (createSceneView True (anchorLayout [AnchorConstraint (Just 50) (Just 50) Nothing Nothing]) handleEvent (Just masterScene))
            [   Node (createSceneView False (fixedLayout (GL.Size 250 250)) handleEvent  (Just radarScene)) []
            ]
        ,   Node (createSceneView False defaultLayout handleEvent (Just shadowScene)) []
        ,   Node (createSceneView False defaultLayout handleEvent (Just positionScene)) []
        ,   Node (createSceneView False defaultLayout handleEvent (Just normalScene)) []
        ,   Node (createSceneView False defaultLayout handleEvent (Just albedoAndSpecularScene)) []
        ]

    sizeRef <- newIORef size
    quitRef <- newIORef False

    GLFW.setWindowSizeCallback window $ Just $ \_ w h ->
        sizeRef $= GL.Size (fromIntegral w) (fromIntegral h)

    let doProcessEvent event = do
            ui <- GL.get uiRef
            ui' <- processEvent ui event
            uiRef $= ui'
            quitRef $= uiTerminated ui'

    GLFW.setKeyCallback window $ Just $ \w k n ks mk ->
        doProcessEvent (EventKey k n ks mk)
    GLFW.setMouseButtonCallback window $ Just $ \w b bs mk ->
        doProcessEvent (EventMouseButton b bs mk)
    GLFW.setCursorPosCallback window $ Just $ \w x y ->
        doProcessEvent (EventCursorPos x y)

    GLFW.swapInterval 1 -- VSync on

    resources2 <- GL.get currentResources
    resources3 <- flip execStateT resources2 $ do
        -- run the main loop
        mainLoop window currentWorld uiRef sizeRef quitRef (0, Nothing, Nothing)

        -- cleaning
        world <- GL.get currentWorld
        disposeWorld world
        context <- GL.get currentContext
        disposeContext context

    GLFW.destroyWindow window
    GLFW.terminate

mainLoop
    :: GLFW.Window
    -> IORef World
    -> IORef SceneUI
    -> IORef GL.Size
    -> IORef Bool
    -> (Int, Maybe Double, Maybe Double)
    -> ResourceIO ()
mainLoop window worldRef uiRef sizeRef quitRef (frameCount, mt0, mt1) = do
    mt2 <- liftIO GLFW.getTime
    let elapsedSeconds = fromMaybe 1 ((-) <$> mt2 <*> mt0)
    (fps, timing) <- if elapsedSeconds > 0.25
        then do
            let fps = fromIntegral frameCount / elapsedSeconds
            liftIO $ GLFW.setWindowTitle window ("Kage - OpenGL @ FPS: " ++ showGFloat (Just 2) fps "")
            return (Just fps, (0, mt2, mt2))
        else
            return (Nothing, (frameCount + 1, mt0, mt2))

    size@(GL.Size w h) <- GL.get sizeRef
    uiRef $~ layout size

    let frameDuration = fromMaybe 0 ((-) <$> mt2 <*> mt1)
    GL.get worldRef >>= animateWorld frameDuration >>= ($=) worldRef
    GL.get uiRef >>= animateUI frameDuration >>= ($=) uiRef

    ui <- GL.get uiRef
    when (w > 0) $ liftIO $ do -- avoid an upcoming divide by zero
        GL.clear [GL.ColorBuffer]
        renderViewTree size (uiRoot ui)
        GLFW.swapBuffers window

    liftIO GLFW.pollEvents
    shouldQuit <- liftIO $ (||) <$> GL.get quitRef <*> GLFW.windowShouldClose window
    if shouldQuit
        then liftIO $ infoM "Kage" "Exiting"
        else mainLoop window worldRef uiRef sizeRef quitRef timing

animateUI :: Double -> SceneUI -> ResourceIO SceneUI
animateUI frameDuration ui = do
    root' <- forM (uiRoot ui) $ \view -> do
        let (_, size) = viewLocalBounds view
        c <- case viewContent view of
            Just scene -> Just <$> sceneAnimate scene size frameDuration
            Nothing -> return Nothing
        return view{ viewContent = c }
    return $ ui{ uiRoot = root' }

renderViewTree :: GL.Size -> Tree (View (Maybe Scene)) -> IO ()
renderViewTree screenSize tree = do
    let (GL.Size screenWidth screenHeigh) = screenSize
        view = rootLabel tree
    case viewContent view of
        Just scene -> do
            let (GL.Position x y, GL.Size w h) = viewLocalBounds view
            GL.viewport $= (GL.Position x (screenHeigh - h - y), GL.Size w h)
            GL.clear [GL.DepthBuffer]
            sceneRender scene (GL.Size w h)
            GL.flush
        _ -> return ()
    mapM_ (renderViewTree screenSize) (subForest tree)
