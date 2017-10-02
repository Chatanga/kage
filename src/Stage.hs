module Stage (
    Event(..),
    Scene(..),
    runScene
) where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.Maybe
import Data.IORef
import Numeric (showGFloat)
import System.Log.Logger

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (get, ($=),($~),($~!)) -- could also be used with IORef
import qualified Graphics.UI.GLFW as GLFW

----------------------------------------------------------------------------------------------------

data Event
    = EventMouseButton  !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
    | EventCursorPos    !Double !Double
    | EventKey          !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
    | EventChar         !Char
    deriving Show

data Scene = Scene
    { sceneRender :: GL.Size -> IO ()
    , sceneAnimate :: GL.Size -> Double -> IO Scene
    , sceneManipulate :: GL.Position -> GL.Size -> Event -> IO (Maybe Scene)
    }

runScene :: (String, IO Scene) -> IO ()
runScene (name, sceneSupplier) = do
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

    scene <- sceneSupplier

    currentScene <- newIORef scene
    currentSize <- newIORef size
    quit <- newIORef False

    GLFW.setWindowSizeCallback window $ Just $ \_ w h ->
        currentSize $= GL.Size (fromIntegral w) (fromIntegral h)

    let processEvent event = do
            scene <- get currentScene
            size <- get currentSize
            result <- sceneManipulate scene (GL.Position 0 0) size event
            case result of
                Just newScene -> currentScene $= newScene
                Nothing -> quit $= True

    GLFW.setKeyCallback window $ Just $ \w k n ks mk ->
        processEvent (EventKey k n ks mk)
    GLFW.setMouseButtonCallback window $ Just $ \w b bs mk ->
        processEvent (EventMouseButton b bs mk)
    GLFW.setCursorPosCallback window $ Just $ \w x y ->
        processEvent (EventCursorPos x y)

    GLFW.swapInterval 1 -- VSync on

    -- run the main loop
    mainLoop window currentScene currentSize quit (0, Nothing, Nothing)

    GLFW.destroyWindow window
    GLFW.terminate

mainLoop
    :: GLFW.Window
    -> IORef Scene
    -> IORef GL.Size
    -> IORef Bool
    -> (Int, Maybe Double, Maybe Double)
    -> IO ()
mainLoop window currentScene currentSize quit (frameCount, mt0, mt1) = do
    mt2 <- GLFW.getTime
    let elapsedSeconds = fromMaybe 1 ((-) <$> mt2 <*> mt0)
    (fps, timing) <- if elapsedSeconds > 0.25
        then do
            let fps = fromIntegral frameCount / elapsedSeconds
            GLFW.setWindowTitle window ("OpenGL @ FPS: " ++ showGFloat (Just 2) fps "")
            return (Just fps, (0, mt2, mt2))
        else
            return (Nothing, (frameCount + 1, mt0, mt2))

    let frameDuration = fromMaybe 0 ((-) <$> mt2 <*> mt1)
    size <- get currentSize
    scene <- get currentScene
    scene' <- sceneAnimate scene size frameDuration
    currentScene $= scene'

    scene <- get currentScene
    let (GL.Size w h) = size
    when (w > 0) $ do -- avoid an upcoming divide by zero
        let hw = w `div` 2
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        -- begin stack layout (depth)
        -- begin flat layout (color)
        GL.viewport $= (GL.Position 0 0, GL.Size hw h)
        sceneRender scene (GL.Size hw h)
        GL.viewport $= (GL.Position hw 0, GL.Size (w - hw) h)
        sceneRender scene (GL.Size (w - hw) h)
        GL.viewport $= (GL.Position (w - 180) (h - 140), GL.Size 160 120)
        -- end flat layout
        GL.clear [GL.DepthBuffer]
        sceneRender scene (GL.Size 80 60)
        -- end stack layout
        GL.flush
        GLFW.swapBuffers window

    GLFW.pollEvents
    shouldClose <- (||) <$> get quit <*> GLFW.windowShouldClose window

    unless shouldClose $
        mainLoop window currentScene currentSize quit timing
