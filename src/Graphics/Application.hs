module Graphics.Application (
    runDefaultApplication,
    runAnotherApplication
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

import Graphics.FunctionalGL
import Graphics.Layouts
import Graphics.Scene
import Graphics.View
import Graphics.World

----------------------------------------------------------------------------------------------------

type SceneUI = UI (Maybe Scene)

createSceneView :: Bool -> Layout (Maybe Scene) -> ViewHandleEvent (Maybe Scene) -> Maybe Scene -> View (Maybe Scene)
createSceneView hasFocus layout handleEvent content = (createView content)
    { viewHandleEvent = handleEvent
    , viewLayout = layout
    , viewHasFocus = hasFocus
    }

{- | Handle an event through simple delegation to its inner content scene, if any. A scene being not
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

runDefaultApplication :: String -> IO ()
runDefaultApplication name = runApplication name createWorld createSceneUI

runAnotherApplication :: String -> IO ()
runAnotherApplication name = runApplication name createAnotherWorld createAnotherSceneUI

runApplication
    :: String
    -> (ResourceIO World)
    -> (IORef World -> IORef Context -> IORef ResourceMap -> SceneUI)
    -> IO ()
runApplication name worldBuilder sceneBuilder = do
    let (w , h) = (800, 600)
        size = GL.Size (fromIntegral w) (fromIntegral h)

    (GLFW.Version x y z) <- GLFW.getVersion
    infoM "Kage" ("Starting GLFW " ++ show x ++ "." ++ show y ++ "." ++ show z)

    GLFW.setErrorCallback (Just (\e s -> errorM "Kage" (show e ++ ": " ++ show s)))

    True <- GLFW.init
    mapM_ GLFW.windowHint
        [   GLFW.WindowHint'ContextVersionMajor 4 --
        ,   GLFW.WindowHint'ContextVersionMinor 3 -- OpenGL >= 4.3 (required for compute shaders)
        ,   GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
        ,   GLFW.WindowHint'Samples 4 -- 4x antialiasing (not very useful in deferred shading)
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
        currentWorld <- liftIO . newIORef =<< worldBuilder
        currentContext <- liftIO . newIORef =<< createContext
        return (currentWorld, currentContext)

    currentResources <- newIORef resources1

    uiRef <- newIORef (sceneBuilder currentWorld currentContext currentResources)

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

    GL.get currentResources >>= execStateT (do
        -- run the main loop
        mainLoop window currentWorld uiRef sizeRef quitRef (0, Nothing, Nothing)

        -- cleaning
        world <- GL.get currentWorld
        disposeWorld world
        context <- GL.get currentContext
        disposeContext context
        releaseUnreclaimedResources)

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
    when (w > 0) $ do -- avoid an upcoming divide by zero
        liftIO $ GL.clear [GL.ColorBuffer]
        renderViewTree size (uiRoot ui)
        liftIO $ GLFW.swapBuffers window

    liftIO GLFW.pollEvents
    shouldQuit <- liftIO $ (||) <$> GL.get quitRef <*> GLFW.windowShouldClose window
    if shouldQuit
        then liftIO $ infoM "Kage" "Exiting"
        else mainLoop window worldRef uiRef sizeRef quitRef timing

mf :: (b -> Bool) -> (a -> b) -> [a] -> [b]
mf = (. map) . (.) . filter

createSceneUI
    :: IORef World
    -> IORef Context
    -> IORef ResourceMap
    -> SceneUI
createSceneUI currentWorld currentContext currentResources = createUI ui where
    masterScene = createScene DeferredRendering currentWorld currentContext
    radarScene = createScene SimpleRendering currentWorld currentContext
    [ssaoScene, positionScene, normalScene, albedoAndSpecularScene, shadowScene] =
        map (`createColorBufferScene` currentContext) [0..4]

    handleEvent = sceneHandleEvent currentResources

    ui =
        Node (createSceneView False adaptativeLayout handleEvent Nothing)
        [   Node (createSceneView True (anchorLayout [AnchorConstraint (Just 50) (Just 50) Nothing Nothing]) handleEvent (Just masterScene))
            [   Node (createSceneView False (fixedLayout (GL.Size 250 250)) handleEvent  (Just radarScene)) []
            ]
        ,   Node (createSceneView False defaultLayout handleEvent (Just ssaoScene)) []
        ,   Node (createSceneView False defaultLayout handleEvent (Just positionScene)) []
        ,   Node (createSceneView False defaultLayout handleEvent (Just normalScene)) []
        ,   Node (createSceneView False defaultLayout handleEvent (Just albedoAndSpecularScene)) []
        ,   Node (createSceneView False defaultLayout handleEvent (Just shadowScene)) []
        ]

createAnotherSceneUI
    :: IORef World
    -> IORef Context
    -> IORef ResourceMap
    -> SceneUI
createAnotherSceneUI currentWorld currentContext currentResources = createUI ui where
    masterScene = createScene SimpleRendering currentWorld currentContext
    handleEvent = sceneHandleEvent currentResources
    ui = Node (createSceneView True defaultLayout handleEvent (Just masterScene)) []

animateUI :: Double -> SceneUI -> ResourceIO SceneUI
animateUI frameDuration ui = do
    root' <- forM (uiRoot ui) $ \view -> do
        let (_, size) = viewLocalBounds view
        c <- case viewContent view of
            Just scene -> Just <$> sceneAnimate scene size frameDuration
            Nothing -> return Nothing
        return view{ viewContent = c }
    return $ ui{ uiRoot = root' }

renderViewTree :: GL.Size -> Tree (View (Maybe Scene)) -> ResourceIO ()
renderViewTree screenSize tree = do
    let (GL.Size screenWidth screenHeigh) = screenSize
        view = rootLabel tree
    case viewContent view of
        Just scene -> do
            let (GL.Position x y, GL.Size w h) = viewLocalBounds view
            liftIO $ GL.viewport $= (GL.Position x (screenHeigh - h - y), GL.Size w h)
            liftIO $ GL.clear [GL.DepthBuffer]
            sceneRender scene (GL.Size w h)
            liftIO GL.flush
        _ -> return ()
    mapM_ (renderViewTree screenSize) (subForest tree)

