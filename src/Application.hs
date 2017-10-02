module Application (
    Event(..),
    Scene(..),
    run
) where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.Maybe
import Data.IORef
import qualified Data.Set as Set
import Numeric (showGFloat)
import System.Log.Logger
import Data.Tree
import Data.Tree.Zipper

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (get, ($=),($~),($~!)) -- could also be used with IORef
import qualified Graphics.UI.GLFW as GLFW
import Linear as LP

import View
import World
import Scene
import FunctionalGL

----------------------------------------------------------------------------------------------------

type SceneUI = UI (Maybe Scene)

run :: String -> IO ()
run name = do
    let (w , h) = (800, 600)
        size = GL.Size (fromIntegral w) (fromIntegral h)

    (GLFW.Version x y z) <- GLFW.getVersion
    infoM "Kage" ("Starting GLFW " ++ show x ++ "." ++ show y ++ "." ++ show z)

    GLFW.setErrorCallback (Just (\e s -> errorM "Kage" (show e ++ ": " ++ show s)))

    True <- GLFW.init
    mapM_ GLFW.windowHint
        [   GLFW.WindowHint'Samples 4 -- 4x antialiasing
        ,   GLFW.WindowHint'ContextVersionMajor 4 --
        ,   GLFW.WindowHint'ContextVersionMinor 3 -- OpenGL >= 4.3 (required for compute shaders)
        ,   GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
        ]
    (Just window) <- GLFW.createWindow w h name Nothing Nothing
    GLFW.makeContextCurrent (Just window)
    GLFW.setWindowSize window w h -- must happen *after* 'createWindow'

    mapM_ (\(k, v) -> get v >>= infoM "Kage" . ((k ++ ": ") ++))
        [   ("Renderer", GL.renderer)
        ,   ("OpenGL version", GL.glVersion)
        ,   ("Shading language version", GL.shadingLanguageVersion)
        ]

    currentWorld <- newIORef =<< initWorld
    currentContext <- newIORef =<< initContext
    let masterScene = initScene DeferredShading currentWorld currentContext
        radarScene = initScene ForwardShading currentWorld currentContext
        [shadowScene, positionScene, normalScene, albedoAndSpecularScene] =
            map (`initColorBufferScene` currentContext) [0..3]

    currentSize <- newIORef size
    needLayout <- newIORef False
    quit <- newIORef False

    currentUI <- newIORef . createUI $
        Node (createSceneView False adaptativeLayout Nothing)
        [   Node (createSceneView True (anchorLayout [AnchorConstraint (Just 50) (Just 50) Nothing Nothing]) (Just masterScene))
            [   Node (createSceneView False (fixedLayout (GL.Size 150 150)) (Just radarScene)) []
            ]
        ,   Node (createSceneView False defaultLayout (Just shadowScene)) []
        ,   Node (createSceneView False defaultLayout (Just positionScene)) []
        ,   Node (createSceneView False defaultLayout (Just normalScene)) []
        ,   Node (createSceneView False defaultLayout (Just albedoAndSpecularScene)) []
        ]

    GLFW.setWindowSizeCallback window $ Just $ \_ w h -> do
        currentSize $= GL.Size (fromIntegral w) (fromIntegral h)
        needLayout $= True

    let doProcessEvent event = do
            ui <- get currentUI
            size <- get currentSize
            ui' <- processEvent ui event
            currentUI $= ui'
            quit $= uiClosing ui'

    GLFW.setKeyCallback window $ Just $ \w k n ks mk ->
        doProcessEvent (EventKey k n ks mk)
    GLFW.setMouseButtonCallback window $ Just $ \w b bs mk ->
        doProcessEvent (EventMouseButton b bs mk)
    GLFW.setCursorPosCallback window $ Just $ \w x y ->
        doProcessEvent (EventCursorPos x y)

    GLFW.swapInterval 1 -- VSync on

    -- run the main loop
    mainLoop window currentWorld currentUI currentSize quit (0, Nothing, Nothing)

    -- Cleaning
    world <- get currentWorld
    mapM_ o3d_dispose (fst (worldFireBalls world) : worldObjects world)

    GLFW.destroyWindow window
    GLFW.terminate

createSceneView :: Bool -> Layout (Maybe Scene) -> Maybe Scene -> View (Maybe Scene)
createSceneView hasFocus layout content = View
    (GL.Position 0 0, GL.Size 0 0)
    content
    hasFocus
    False
    []
    sceneHandleEvent
    layout

{- | Handle an event throw simple delegation to its inner content scene, if any. A scene being not
aware of the view holding it, any change will stay local (excepted for a Nothing interpreted as a
closure).
-}
sceneHandleEvent :: ViewHandleEvent (Maybe Scene)
sceneHandleEvent event treeLoc = do
    let view = getLabel treeLoc
        scene = viewContent view
        (_, size) = viewLocalBounds view
    case scene of
        Nothing -> return Nothing
        Just scene -> do
            content' <- sceneManipulate scene size event
            -- putStrLn $ "handle -> " ++ show event
            case content' of
                Nothing -> return (Just (sendEvent EventClose treeLoc))
                Just scene' ->
                    let view' = view{ viewContent = Just scene' }
                    in  return (Just (modifyLabel (const view') treeLoc))

mainLoop
    :: GLFW.Window
    -> IORef World
    -> IORef SceneUI
    -> IORef GL.Size
    -> IORef Bool
    -> (Int, Maybe Double, Maybe Double)
    -> IO ()
mainLoop window currentWorld currentUI currentSize quit (frameCount, mt0, mt1) = do
    mt2 <- GLFW.getTime
    let elapsedSeconds = fromMaybe 1 ((-) <$> mt2 <*> mt0)
    (fps, timing) <- if elapsedSeconds > 0.25
        then do
            let fps = fromIntegral frameCount / elapsedSeconds
            GLFW.setWindowTitle window ("OpenGL @ FPS: " ++ showGFloat (Just 2) fps "")
            return (Just fps, (0, mt2, mt2))
        else
            return (Nothing, (frameCount + 1, mt0, mt2))

    size@(GL.Size w h) <- get currentSize
    currentUI $~ layout size

    let frameDuration = fromMaybe 0 ((-) <$> mt2 <*> mt1)
    get currentWorld >>= animateWorld frameDuration >>= ($=) currentWorld
    get currentUI >>= animateUI frameDuration >>= ($=) currentUI

    ui <- get currentUI
    when (w > 0) $ do -- avoid an upcoming divide by zero
        GL.clear [GL.ColorBuffer]
        renderView size (uiRoot ui)
        -- GL.flush
        GLFW.swapBuffers window

    GLFW.pollEvents
    shouldClose <- (||) <$> get quit <*> GLFW.windowShouldClose window
    when shouldClose $ infoM "Kage" "Exiting"

    unless shouldClose $
        mainLoop window currentWorld currentUI currentSize quit timing

animateUI :: Double -> SceneUI -> IO SceneUI
animateUI timeDelta ui = do
    root' <- forM (uiRoot ui) $ \view -> do
        let (_, size) = viewLocalBounds view
        c <- case viewContent view of
            Just scene -> Just <$> sceneAnimate scene size timeDelta
            Nothing -> return Nothing
        return view{ viewContent = c }
    return $ ui{ uiRoot = root' }

renderView :: GL.Size -> Tree (View (Maybe Scene)) -> IO ()
renderView (GL.Size screenWidth screenHeigh) tree = do
    let view = rootLabel tree
    case viewContent view of
        Just scene -> do
            let (GL.Position x y, GL.Size w h) = viewLocalBounds view
            GL.viewport $= (GL.Position x (screenHeigh - h - y), GL.Size w h)
            GL.clear [GL.DepthBuffer]
            sceneRender scene (GL.Size w h)
            GL.flush
        _ -> return ()
    mapM_ (renderView (GL.Size screenWidth screenHeigh)) (subForest tree)
