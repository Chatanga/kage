module Graphics.Scene (
    Scene(..),
    Context(..),
    RenderingMode(..),
    createContext,
    disposeContext,
    createScene,
    createColorBufferScene
) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.State as S
import Data.Either
import Data.Fixed
import Data.IORef
import Data.List (intercalate, sortBy)
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set
import qualified Data.Vector.Storable as VS
import Foreign (nullPtr)
import qualified GHC.Float as F
-- import qualified Graphics.Rendering.FTGL as FTGL
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Linear
import Numeric (showGFloat)
import System.Log.Logger

import Common.Debug
import Common.Misc
import Common.Random

import Graphics.Buffer
import Graphics.Catalog
import Graphics.Error
import Graphics.Ext.Program
import Graphics.Ext.Shader
import Graphics.Font
import Graphics.FunctionalGL
import Graphics.Geometry
import Graphics.Heightmap
import Graphics.Shader
import Graphics.Texture
import Graphics.View
import Graphics.World

----------------------------------------------------------------------------------------------------

data RenderingMode = SimpleRendering | DirectRendering | DeferredRendering

useShadowMap = True
shadowMapSize = Size 4096 4096

data FrameBufferSet = FrameBufferSet
    {   fbsSize :: Size
    ,   fbsGeometry :: (FramebufferObject, [TextureObject])
    ,   fbsSsao ::  (FramebufferObject, TextureObject)
    ,   fbsBluredSsao :: (FramebufferObject, TextureObject)
    ,   fbsHdrColor :: (FramebufferObject, TextureObject)
    ,   fbsToneMapping :: (FramebufferObject, TextureObject, TextureObject)
    ,   fbsBlurredBloom :: (FramebufferObject, TextureObject)
    ,   fbsBlurredBloomAlt :: (FramebufferObject, TextureObject)
    ,   fbsDispose :: Dispose
    }

data ScreenType
    = Generic
    | Simple
    | Ssao
    | Blur
    | Lighting -- HDR
    | ToneMapping
    | ColorBlur -- Gaussian
    | ColorCombine
    deriving (Eq, Ord, Show)

data Context = Context
    {   contextShadowFrameBuffer :: (FramebufferObject, TextureObject, Dispose)
    ,   contextFrameBufferSet :: Maybe FrameBufferSet
    ,   contextScreens :: [(ScreenType, Renderable)]
    ,   contextTitles :: [(String, Renderable)]
    ,   contextCameraName :: String
    ,   contextCameraMoves :: !(Set.Set Move)
    ,   contextCursorPosition :: V2 Double
    ,   contextDrag :: Maybe (V2 Double)
    ,   contextSampleKernel :: [V3 Float]
    ,   contextNoiseTexture :: (TextureObject, Dispose)
    }

data Move
    = GoUp
    | GoDown
    | GoLeft
    | GoRight
    | GoForth
    | GoBack
    deriving (Eq, Ord, Show)

data Scene = Scene
    {   sceneRender :: Size -> ResourceIO ()
    ,   sceneAnimate :: Size -> Double -> ResourceIO Scene
    ,   sceneManipulate :: Size -> Event -> ResourceIO (Maybe Scene) -- nothing <=> exit
    }

createScene :: RenderingMode -> IORef World -> IORef Context -> Scene
createScene renderingMode worldRef contextRef = Scene
    (display renderingMode worldRef contextRef)
    (animate renderingMode worldRef contextRef)
    (manipulate renderingMode worldRef contextRef)

createColorBufferScene :: Int -> IORef Context -> Scene
createColorBufferScene index contextRef = Scene
    (liftIO . displayBuffer contextRef index)
    (\_ _ -> return (createColorBufferScene index contextRef))
    (\_ _ -> return (Just (createColorBufferScene index contextRef)))

createContext :: ResourceIO Context
createContext = do
    shadowFrameBuffer <- liftIO $ createShadowFrameBuffer shadowMapSize
    screens <- mapM (\(t, constructor) -> (,) t <$> constructor)
            [ (Generic, createScreen)
            , (Simple, createSimpleScreen)
            , (Ssao, createSsaoScreen)
            , (Blur, createBlurScreen)
            , (Lighting, createLightingScreen)
            , (ToneMapping, createToneMappingScreen)
            , (ColorBlur, createColorBlurScreen)
            , (ColorCombine, createColorCombineScreen)
            ]
    titles <- mapM (\text -> (,) text <$> createText (V3 (-0.9) (-0.85) 0) (V3 0 0.005 0) (V3 0.0025 0 0) text)
            [ "Blurred SSAO"
            , "Position"
            , "Normal"
            , "Albedo+Spec"
            , "Shadow"
            ] :: ResourceIO [(String, Renderable)]
    sampleKernel <- liftIO generateSampleKernel
    noiseTexture <- liftIO generateNoiseTexture
    let context = Context
            shadowFrameBuffer
            Nothing
            screens
            titles
            "first-camera"
            Set.empty
            (V2 0 0)
            Nothing
            sampleKernel
            noiseTexture
    return context

disposeContext :: Context -> ResourceIO ()
disposeContext context = do
    let (_, _, disposeShadowFrameBuffer) = contextShadowFrameBuffer context
    liftIO disposeShadowFrameBuffer
    mapM_ (renderableDispose . snd) (contextScreens context)
    mapM_ (renderableDispose . snd) (contextTitles context)
    liftIO (snd (contextNoiseTexture context))
    case contextFrameBufferSet context of
        Just fbs -> liftIO (fbsDispose fbs)
        Nothing -> return ()

----------------------------------------------------------------------------------------------------

allJoysticks =  [ GLFW.Joystick'1
                , GLFW.Joystick'2
                , GLFW.Joystick'3
                , GLFW.Joystick'4
                , GLFW.Joystick'5
                , GLFW.Joystick'6
                , GLFW.Joystick'7
                , GLFW.Joystick'8
                , GLFW.Joystick'9
                , GLFW.Joystick'10
                , GLFW.Joystick'11
                , GLFW.Joystick'12
                , GLFW.Joystick'13
                , GLFW.Joystick'14
                , GLFW.Joystick'15
                , GLFW.Joystick'16
                ]

animate :: RenderingMode -> IORef World -> IORef Context -> Size -> Double -> ResourceIO Scene
animate renderingMode worldRef contextRef (Size width height) timeDelta = do
    world <- GL.get worldRef
    context <- GL.get contextRef

    let controlAircraft aircraft = do
            availableJoysticks <- filterM GLFW.joystickPresent allJoysticks
            case listToMaybe availableJoysticks of
                Nothing -> return aircraft
                Just joystick -> do
                    maybeAxes <- GLFW.getJoystickAxes joystick
                    case maybeAxes of
                        Nothing -> return aircraft
                        Just [roll, pitch, yaw, throttle, xHat, yHat] ->
                            return $ aircraft
                                {   aircraftThrustControl = realToFrac throttle
                                ,   aircraftAileronControl = realToFrac roll
                                ,   aircraftElevatorControl = realToFrac pitch
                                ,   aircraftRudderControl = realToFrac yaw
                                }

    aircrafts <- liftIO $ mapM controlAircraft (worldAircrafts world)

    -- Move a camera for real by applying any registered move for the given time delta.
    let moveCamera camera = camera
                { cameraAltitude = alt'
                , cameraAzimuth = az'
                , cameraPosition = position }
            where
                moves = contextCameraMoves context
                keyMoves =
                    [ (GoUp, getUp camera)
                    , (GoDown, - (getUp camera))
                    , (GoLeft, getLeft camera)
                    , (GoRight, - (getLeft camera))
                    , (GoForth, getSight camera)
                    , (GoBack, - (getSight camera))
                    ]
                applyMove p (m, dp) = if Set.member m moves
                    then p + dp * realToFrac timeDelta * 10
                    else p
                position = foldl applyMove (cameraPosition camera) keyMoves
                (alt, az) = (cameraAltitude camera, cameraAzimuth camera)
                (alt', az') = case contextDrag context of
                    Nothing -> (alt, az)
                    Just drag -> (alt', az') where
                        ratio = cameraFov camera / realToFrac height :: GLfloat
                        V2 dAz dAlt = (realToFrac <$> drag) * V2 ratio ratio
                        alt' = alt + clamp (-pi/2.01-alt) (pi/2.01-alt) dAlt
                        az' = mod' (az - dAz) (2 * pi)

    let cameras = for (worldCameras world) $ \(n, c) ->
            if n == contextCameraName context then (n, moveCamera c) else (n, c)

    worldRef $= world{ worldAircrafts = aircrafts, worldCameras = cameras }

    contextRef $= context{ contextDrag = Nothing }

    return $ createScene renderingMode worldRef contextRef

----------------------------------------------------------------------------------------------------

manipulate :: RenderingMode -> IORef World -> IORef Context -> Size -> Event -> ResourceIO (Maybe Scene)

-- Handle keyboard events.
manipulate renderingMode worldRef contextRef size (EventKey k _ ks _)

    -- Exit application on Escape.
    | k == GLFW.Key'Escape = return Nothing

    -- Dump the shadow map in a PNG on F1.
    | k == GLFW.Key'F1 && ks == GLFW.KeyState'Pressed = do
        context <- GL.get contextRef
        let Size w h = shadowMapSize
            (_, shadowTexture, _) = contextShadowFrameBuffer context
        liftIO $ saveDepthTexture (fromIntegral w, fromIntegral h) shadowTexture "shadowmap.png"
        return $ Just (createScene renderingMode worldRef contextRef)

    -- Move the camera using WASD keys (note that the keyboard layout is not taken into account).
    | otherwise = do
        context <- GL.get contextRef
        let
            moves = contextCameraMoves context
            handleKey = if ks == GLFW.KeyState'Released then Set.delete else Set.insert
            moves' = case k of
                GLFW.Key'Space -> handleKey GoUp moves
                GLFW.Key'LeftControl -> handleKey GoDown moves
                GLFW.Key'W -> handleKey GoForth moves
                GLFW.Key'S -> handleKey GoBack moves
                GLFW.Key'A -> handleKey GoLeft moves
                GLFW.Key'D -> handleKey GoRight moves
                _ -> moves
        contextRef $= context{ contextCameraMoves = moves' }
        return $ Just (createScene renderingMode worldRef contextRef)

-- Rotate the camera by dragging the mouse.
manipulate renderingMode worldRef contextRef size (EventDrag dx dy) = do
    context <- GL.get contextRef
    let drag = fromMaybe (V2 0 0) (contextDrag context) + V2 dx dy
    contextRef $= context{ contextDrag = Just drag }
    return $ Just (createScene renderingMode worldRef contextRef)

-- Shot randomly colored fire balls with the mouse right button.
manipulate renderingMode worldRef contextRef size (EventMouseButton b bs _) = do
    world <- GL.get worldRef
    context <- GL.get contextRef
    let camera = fromJust (lookup (contextCameraName context) (worldCameras world))
    newFireBalls <- if b == GLFW.MouseButton'2 && bs == GLFW.MouseButtonState'Pressed
        then do
            let (Size width height) = size
                projectionMatrix = Linear.perspective (cameraFov camera) (width `divR` height) near far
                cameraMatrix = Linear.lookAt
                    (cameraPosition camera)
                    (cameraPosition camera + getSight camera)
                    (getUp camera)
                cursor = contextCursorPosition context
                (V4 x y z _) = toWorld (Size width height) cursor projectionMatrix cameraMatrix
                direction = Linear.normalize (V3 x y z - cameraPosition camera)
            color <- liftIO $ runRandomIO $ Color3
                <$> getRandomR (0, 1)
                <*> getRandomR (0, 1)
                <*> getRandomR (0, 1)
            return [FireBall (cameraPosition camera) direction color 0]
        else return []
    worldRef $= world{ worldFireBalls = newFireBalls ++ worldFireBalls world }
    contextRef $= context
    return $ Just (createScene renderingMode worldRef contextRef)

-- Store the cursor location.
manipulate renderingMode worldRef contextRef size (EventCursorPos x y) = do
    world <- GL.get worldRef
    context <- GL.get contextRef
    contextRef $= context{ contextCursorPosition = V2 x y }
    return $ Just (createScene renderingMode worldRef contextRef)

-- Catch everything else.
manipulate renderingMode worldRef contextRef size _ =
    return $ Just (createScene renderingMode worldRef contextRef)

----------------------------------------------------------------------------------------------------

display :: RenderingMode -> IORef World -> IORef Context -> Size -> ResourceIO ()
display renderingMode worldRef contextRef size = do
    world <- liftIO $ GL.get worldRef

    fireBallObjects <- concat <$> mapM instanciate (worldFireBalls world)
    aircraftObjects <- concat <$> mapM instanciate (worldAircrafts world)

    let lights = map (\(FireBall p d c a) -> PointLight p c) (worldFireBalls world)
        objects = worldObjects world ++ fireBallObjects ++ aircraftObjects
        elapsedTime = worldElapsedTime world

    liftIO $ do
        -- Global?
        clearColor $= Color4 0 0 0 1
        depthFunc $= Just Less

        shadowInfo <- if useShadowMap
            then Just <$> renderShadow world contextRef shadowMapSize objects
            else return Nothing

        renderScene renderingMode world contextRef size shadowInfo lights objects

        printErrors "Errors"

renderShadow
    :: World
    -> IORef Context
    -> Size
    -> [Renderable]
    -> IO (GLmatrix GLfloat, TextureObject)
renderShadow world contextRef size objects = do
    context <- GL.get contextRef

    let camera = fromJust (lookup (contextCameraName context) (worldCameras world))
        sun = worldSun world
        r = far / 8
        projectionMatrix = Linear.ortho (-r / 2) (r / 2) (-r) r (-r) r
        cameraMatrix = Linear.lookAt
            (cameraPosition camera - directionLightDirection sun)
            (cameraPosition camera)
            (getUp camera)
            :: M44 GLfloat
        biasMatrix = V4
            (V4 0.5 0.0 0.0 0.5)
            (V4 0.0 0.5 0.0 0.5)
            (V4 0.0 0.0 0.5 0.5)
            (V4 0.0 0.0 0.0 1.0)
        shadowMatrix = biasMatrix !*! projectionMatrix !*! cameraMatrix

    projectionMat <- newMatrix RowMajor (flattenMatrix projectionMatrix)
    cameraMat <- newMatrix RowMajor (flattenMatrix cameraMatrix)

    let (fbo, texture, _) = contextShadowFrameBuffer context

    backupViewport <- GL.get viewport
    viewport $= (Position 0 0, size)
    withDrawFramebuffer fbo $ do
        clear [DepthBuffer]
        cullFace $= Nothing
        frontFace $= CW
        forM_ objects $ renderObjectShadow
            size
            (worldElapsedTime world)
            projectionMat
            cameraMat
            texture
    viewport $= backupViewport

    shadowMat <- newMatrix RowMajor (flattenMatrix shadowMatrix)
    return (shadowMat, texture)

renderObjectShadow
    :: Size
    -> Double
    -> GLmatrix GLfloat
    -> GLmatrix GLfloat
    -> TextureObject
    -> Renderable
    -> IO ()
renderObjectShadow
    size
    elapsedTime
    projection
    camera
    shadowTexture
    (Renderable programs vao render _ _)
    = case lookup ShadowMappingStage programs of
        Nothing -> return ()
        Just program -> withBinding currentExtProgram program $ do
            setUniform program "camera" camera
            setUniform program "projection" projection
            transformation <- newMatrix RowMajor (flattenMatrix identity) :: IO (GLmatrix GLfloat)
            setUniform program "transformation" transformation
            setUniform program "timePassed" (F.double2Float elapsedTime :: GLfloat)
            setUniform program "stage" (ordinal ShadowMappingStage)
            render program

renderScene
    :: RenderingMode
    -> World
    -> IORef Context
    -> Size
    -> Maybe (GLmatrix GLfloat, TextureObject)
    -> [PointLight]
    -> [Renderable]
    -> IO ()
renderScene renderingMode world contextRef size shadowInfo lights objects = do
    context <- GL.get contextRef

    let (Size width heigh) = size
        camera = fromJust (lookup (contextCameraName context) (worldCameras world))
        camPosition = Vector3 cx cy cz where V3 cx cy cz = cameraPosition camera
        -- FOV (y direction, in radians), Aspect ratio, Near plane, Far plane
        projectionMatrix = Linear.perspective (cameraFov camera) (width `divR` heigh) near far
        -- Eye, Center, Up
        cameraMatrix = Linear.lookAt
            (cameraPosition camera)
            (cameraPosition camera + getSight camera)
            (getUp camera)

    projectionMat <- newMatrix RowMajor (flattenMatrix projectionMatrix)
    cameraMat <- newMatrix RowMajor (flattenMatrix cameraMatrix)

    let isRenderableIn stage (Renderable programs vao render _ _) = isJust (lookup stage programs)
        renderObjectIn = renderObject
            size
            (worldElapsedTime world)
            shadowInfo
            camPosition
            projectionMat
            cameraMat
            (worldSun world)
            lights
        sortedObjects = sortBy (comparing renderableZOrder) objects

    case renderingMode of

        SimpleRendering ->
            mapM_ (renderObjectIn DirectShadingStage) sortedObjects

        _ -> do

            fbs <- getFrameBufferSet contextRef size
            let (fbo4, hdrColorTexture) = fbsHdrColor fbs
                (fbo5, ldrColorTexture, bloomTexture) = fbsToneMapping fbs
                (fbo6, bluredBloomTexture) = fbsBlurredBloom fbs
                (fbo7, bluredBloomTextureAlt) = fbsBlurredBloomAlt fbs

            case renderingMode of

                DirectRendering ->
                    withDrawFramebuffer' size fbo4 $ do
                        clear [ColorBuffer, DepthBuffer]
                        mapM_ (renderObjectIn DirectShadingStage) sortedObjects

                DeferredRendering -> do
                    let (fbo1, geometryTextures) = fbsGeometry fbs
                        (fbo2, ssaoTexture) = fbsSsao fbs
                        (fbo3, bluredSsaoTexture) = fbsBluredSsao fbs

                    -- Deferred rendering
                    withDrawFramebuffer' size fbo1 $ do
                        clear [ColorBuffer, DepthBuffer]
                        mapM_ (renderObjectIn DeferredShadingStage) sortedObjects

                    -- SSAO
                    let Just (Renderable programs vao render dispose zOrder) = lookup Ssao (contextScreens context)
                        renderSsao p = do
                            forM_ (zip [0..] (contextSampleKernel context)) $ \(i, v) ->
                                setUniform p ("samples[" ++ show i ++ "]") (toVector3 v)
                            setUniform p "screenWidth" (fromIntegral width :: Float)
                            setUniform p "screenHeight" (fromIntegral heigh :: Float)
                            let noiseTexture = fst (contextNoiseTexture context)
                            usingOrderedTextures p (noiseTexture : geometryTextures) (render p)
                    withDrawFramebuffer' size fbo2 $
                        renderObjectIn DirectShadingStage (Renderable programs vao renderSsao dispose zOrder)

                    -- Blur
                    let Just (Renderable programs vao render dispose zOrder) = lookup Blur (contextScreens context)
                        renderBlur p = usingOrderedTextures p [ssaoTexture] (render p)
                    withDrawFramebuffer' size fbo3 $
                        renderObjectIn DirectShadingStage (Renderable programs vao renderBlur dispose zOrder)

                    -- Lighting
                    let Just (Renderable programs vao render dispose zOrder) = lookup Lighting (contextScreens context)
                        renderLighting p = usingOrderedTextures p (bluredSsaoTexture : geometryTextures) (render p)
                    withDrawFramebuffer' size fbo4 $ do
                        renderObjectIn DirectShadingStage (Renderable programs vao renderLighting dispose zOrder)
                        -- Copy back the depth buffer from our initial rendering.
                        bindFramebuffer ReadFramebuffer $= fbo1
                        blitFramebuffer
                            (Position 0 0) (Position width heigh)
                            (Position 0 0) (Position width heigh)
                            [DepthBuffer'] Nearest
                        -- Render other objects (especially transparent ones) directly.
                        mapM_ (renderObjectIn DirectShadingStage)
                            (filter (not . isRenderableIn DeferredShadingStage) sortedObjects)

            -- Tone mapping
            let Just (Renderable programs vao render dispose zOrder) = lookup ToneMapping (contextScreens context)
                renderToneMapping p = usingOrderedTextures p [hdrColorTexture] (render p)
            withDrawFramebuffer' size fbo5 $
                renderObjectIn DirectShadingStage (Renderable programs vao renderToneMapping dispose zOrder)

            -- Gaussian blur
            let Just (Renderable programs vao render dispose zOrder) = lookup ColorBlur (contextScreens context)
                operations = take 10 $ (bloomTexture, fbo7, True) : cycle
                    [ (bluredBloomTextureAlt, fbo6, False)
                    , (bluredBloomTexture, fbo7, True)
                    ]
            forM_ operations $ \(texture, fbo, horizontal) ->
                withDrawFramebuffer' size fbo $
                    let renderBlur p = do
                            setUniform p "horizontal" (if horizontal then 1 else 0 :: GLint)
                            usingOrderedTextures p [texture] (render p)
                    in renderObjectIn DirectShadingStage (Renderable programs vao renderBlur dispose zOrder)

            -- Combine
            let Just (Renderable programs vao render dispose zOrder) = lookup ColorCombine (contextScreens context)
                renderCombine p = usingOrderedTextures p [ldrColorTexture, bluredBloomTexture] (render p)
            renderObjectIn DirectShadingStage (Renderable programs vao renderCombine dispose zOrder)

getFrameBufferSet :: IORef Context -> Size -> IO FrameBufferSet
getFrameBufferSet contextRef size = do
    context <- GL.get contextRef
    result <- case contextFrameBufferSet context of
        Just fbs ->
            if fbsSize fbs == size
                then return (Just fbs)
                else fbsDispose fbs >> return Nothing
        Nothing -> return Nothing
    case result of
        Just fbs -> return fbs
        Nothing -> do
            infoM "Kage" ("Creating FB set at size " ++ show size)
            fbs <- createFrameBufferSet size
            contextRef $= context{ contextFrameBufferSet = Just fbs }
            return fbs

createFrameBufferSet :: Size -> IO FrameBufferSet
createFrameBufferSet size = do
    (fbo1, geometryTextures, disposeFramebuffer1) <- createGeometryFrameBuffer False size
    (fbo2, ssaoTexture, disposeFramebuffer2) <- createSimpleFrameBuffer size
    (fbo3, bluredSsaoTexture, disposeFramebuffer3) <- createSimpleFrameBuffer size
    (fbo4, hdrColorTexture, disposeFramebuffer4) <- createHdrFrameBuffer size
    (fbo5, [ldrColorTexture, bloomTexture], disposeFramebuffer5) <- createBloomFrameBuffer size
    (fbo6, bluredBloomTexture, disposeFramebuffer6) <- createColorFrameBuffer size
    (fbo7, bluredBloomTextureAlt, disposeFramebuffer7) <- createColorFrameBuffer size
    let dispose = do
            disposeFramebuffer1
            disposeFramebuffer2
            disposeFramebuffer3
            disposeFramebuffer4
            disposeFramebuffer5
            disposeFramebuffer6
            disposeFramebuffer7
    return $ FrameBufferSet
        size
        (fbo1, geometryTextures)
        (fbo2, ssaoTexture)
        (fbo3, bluredSsaoTexture)
        (fbo4, hdrColorTexture)
        (fbo5, ldrColorTexture, bloomTexture)
        (fbo6, bluredBloomTexture)
        (fbo7, bluredBloomTextureAlt)
        dispose

{-
In https://learnopengl.com/#!Advanced-Lighting/Deferred-Shading:
What we need to do is first copy the depth information stored in the geometry pass into
the default framebuffer's depth buffer and only then render the light cubes. This way
the light cubes' fragments are only rendered when on top of the previously rendered
geometry.
-}
copyDepth :: FramebufferObject -> Size -> IO ()
copyDepth fbo (Size width heigh) = do
    bindFramebuffer ReadFramebuffer $= fbo
    clear [DepthBuffer]
    (Position dx dy) <- fst <$> GL.get viewport
    blitFramebuffer
        (Position 0 0) (Position width heigh)
        (Position dx dy) (Position (dx + width) (dy + heigh))
        [DepthBuffer'] Nearest
    bindFramebuffer Framebuffer $= defaultFramebufferObject

generateSampleKernel :: IO [V3 Float]
generateSampleKernel = replicateM 64 $ do
    [x, y, z, s] <- replicateM 4 (runRandomIO $ getRandomR (0, 1)) :: IO [Float]
    let lerp a b f = a + f * (b - a)
        sample = lerp 0.1 1.0 (s * s) *^ Linear.normalize (V3 (x * 2 - 1) (y * 2 - 1) z)
    return sample :: IO (V3 Float)

generateNoiseTexture :: IO (TextureObject, Dispose)
generateNoiseTexture = do
    let (width, height) = (4, 4)

    noise <- concat <$> replicateM (width * height) (do
        [x, y] <- replicateM 2 (runRandomIO $ getRandomR (0, 1)) :: IO [Float]
        return [x * 2 - 1, y * 2 - 1, 0])

    texture <- genObjectName
    withTexture2D texture $ do
        VS.unsafeWith (VS.fromList noise) $ \ptr ->
            texImage2D
                Texture2D
                NoProxy
                0 -- The mipmap level this image is responsible for.
                RGB16F
                (TextureSize2D (fromIntegral width) (fromIntegral height))
                0 -- No borders
                (PixelData RGB UnsignedByte ptr)
                -- (PixelData RGB Float ptr)
        textureWrapMode Texture2D S $= (Repeated, Repeat)
        textureWrapMode Texture2D T $= (Repeated, Repeat)
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
        return (texture, deleteObjectName texture)

renderObject
    :: Size
    -> Double
    -> Maybe (GLmatrix GLfloat, TextureObject)
    -> Vector3 GLfloat
    -> GLmatrix GLfloat
    -> GLmatrix GLfloat
    -> DirectionLight
    -> [PointLight]
    -> Stage
    -> Renderable
    -> IO ()
renderObject
    size
    elapsedTime
    shadowInfo
    cameraPosition
    projection
    camera
    sun
    lights
    stage
    (Renderable programs vao render _ _)
    = case lookup stage programs of
        Nothing -> return ()
        Just program -> withBinding currentExtProgram program $ do

            setUniform program "cameraPosition" cameraPosition

            setUniform program "projection" projection
            setUniform program "camera" camera
            transformation <- newMatrix RowMajor (flattenMatrix identity) :: IO (GLmatrix GLfloat)
            setUniform program "transformation" transformation

            setUniform program "sunLightColor" (directionLightColor sun)
            setUniform program "sunLightDirection" (toVector3 $ directionLightDirection sun)
            setUniform program "sunLightAmbientIntensity" (directionLightAmbientIntensity sun)

            -- setUniform program "fogColor" (Color4 (144/255) (142/255) (137/255) 1 :: Color4 GLfloat)
            setUniform program "fogColor" (Color4 0 0 0 1 :: Color4 GLfloat)
            setUniform program "fogStart" (50 :: GLfloat)
            setUniform program "fogEnd" (250 :: GLfloat)
            setUniform program "fogDensity" (0.005 :: GLfloat)
            setUniform program "fogEquation" (2 :: GLint) -- 0 = linear, 1 = exp, 2 = exp2

            setUniform program "lightCount" (fromIntegral (length lights) :: GLint)
            forM_ (zip [0..] lights) $ \(index, PointLight (V3 x y z) (Color3 r g b)) -> do
                setUniform program ("lightPositions[" ++ show index ++ "]") (Vector3 x y z)
                setUniform program ("lightColors[" ++ show index ++ "]") (Color3 r g b)

            setUniform program "materialSpecularIntensity" (0 :: GLfloat)
            setUniform program "materialSpecularPower" (0 :: GLfloat)

            setUniform program "timePassed" (F.double2Float elapsedTime :: GLfloat)
            setUniform program "stage" (ordinal stage)

            let contextualizeAction = case shadowInfo of
                    Just (shadow, shadowMap) -> \action p -> do
                        let shadowSampler = TextureUnit 8
                        activeTexture $= shadowSampler
                        textureBinding Texture2D $= Just shadowMap
                        setUniform p "shadowMap" shadowSampler
                        setUniform p "shadow" shadow
                        setUniform p "shadowUsed" (1 :: GLint)
                        action p
                        activeTexture $= shadowSampler
                        textureBinding Texture2D $= Nothing
                    Nothing -> \action p -> do
                        setUniform p "useShadow" (0 :: GLint)
                        action p

            contextualizeAction render program

----------------------------------------------------------------------------------------------------

displayBuffer :: IORef Context -> Int -> Size -> IO ()
displayBuffer contextRef index size = do
    context <- GL.get contextRef

    let renderTexture :: (TextureObject, ScreenType) -> IO ()
        renderTexture (texture, screenType) = case lookup DirectShadingStage programs of
            Nothing -> return ()
            Just program -> withBinding currentExtProgram program .
                usingOrderedTextures program [texture] $
                    render program
            where Just (Renderable programs vao render _ _) = lookup screenType (contextScreens context)

    case contextFrameBufferSet context of
        Just fbs -> do
            let (_, [positionTexture, normalTexture, albedoSpecTexture]) = fbsGeometry fbs
                (_, ssaoTexture) = fbsBluredSsao fbs
                (_, shadowTexture, _) = contextShadowFrameBuffer context
                textures =
                    [ (ssaoTexture, Simple)
                    , (positionTexture, Generic)
                    , (normalTexture, Generic)
                    , (albedoSpecTexture, Generic)
                    , (shadowTexture, Simple)
                    ]
            renderTexture (cycle textures !! index)
        Nothing -> return ()

    let (Renderable programs vao render _ _) = snd (cycle (contextTitles context) !! index)
    case lookup DirectShadingStage programs of
        Nothing -> return ()
        Just program -> withBinding currentExtProgram program $ do
            i <- newMatrix RowMajor (flattenMatrix identity) :: IO (GLmatrix GLfloat)
            setUniform program "camera" i
            setUniform program "projection" i
            setUniform program "transformation" i
            depthFunc $= Nothing
            render program

    printErrors "Errors"
