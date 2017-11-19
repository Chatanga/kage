module Scene (
    Scene(..),
    RenderingMode(..),
    createWorld,
    disposeWorld,
    createContext,
    disposeContext,
    createScene,
    createColorBufferScene
) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Either
import Data.Fixed
import Data.IORef
import Data.List (intercalate)
import Data.Maybe
import qualified Data.Set as Set
import Foreign (nullPtr)
import qualified GHC.Float as F
-- import qualified Graphics.Rendering.FTGL as FTGL
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import Linear
import Numeric (showGFloat)

import Buffer
import Debug
import Error
import Ext.Program
import Ext.Shader
import Font
import FunctionalGL
import Geometry
import Heightmap
import Misc
import Random
import Shader
import Texture
import View
import World

----------------------------------------------------------------------------------------------------

data RenderingMode = ForwardShading | DeferredShading

data Context = Context
    {   contextScreen :: Renderable
    ,   contextDeferredScreen :: Renderable
    ,   contextShadowFrameBuffer :: Maybe (FramebufferObject, TextureObject, Dispose)
    ,   contextGeometryFrameBuffer :: Maybe (FramebufferObject, [TextureObject], Dispose)
    ,   contextCameraName :: String
    ,   contextCameraMoves :: !(Set.Set Move)
    ,   contextCursorPosition :: V2 Double
    ,   contextDrag :: Maybe (V2 Double)
    }

data Move
    = GoUp -- add speed
    | GoDown
    | GoLeft
    | GoRight
    | GoForth
    | GoBack
    | Yaw Double -- dX
    | Pitch Double -- dY
    | Roll Double -- dZ
    deriving (Eq, Ord, Show)

data Scene = Scene
    { sceneRender :: Size -> IO ()
    , sceneAnimate :: Size -> Double -> IO Scene
    , sceneManipulate :: Size -> Event -> IO (Maybe Scene) -- nothing <=> exit
    }

createScene :: RenderingMode -> IORef World -> IORef Context -> Scene
createScene renderingMode worldRef contextRef = Scene
    (display renderingMode worldRef contextRef)
    (animate renderingMode worldRef contextRef)
    (manipulate renderingMode worldRef contextRef)

createColorBufferScene :: Int -> IORef Context -> Scene
createColorBufferScene index contextRef = Scene
    (displayBuffer contextRef index)
    (\_ _ -> return (createColorBufferScene index contextRef))
    (\_ _ -> return (Just (createColorBufferScene index contextRef)))

createWorld :: IO World
createWorld = do
    heightmap <- loadHeightmap "data/heightmap-257.png"
    let heightmapScale = V3 1 1 0.1
    terrain <- createRenderableTerrain heightmap heightmapScale
    objects <- sequence
        [ createSkyBox
        , return terrain
        -- , createNormalDisplaying heightmap terrain
        , createGrass heightmap heightmapScale
        , createSpaceInvader (V3 10 5 25)
        , createSpaceInvader (V3 20 15 15)
        -- , createTesselatedPyramid
        , createText (V3 10 0 20) (V3 0 0 0.04) (V3 0 (-0.04) 0) "Kage　-　かげ"
        ]
    fireBallObject <- createFireBall
    let world = World
            (heightmap, heightmapScale)
            objects
            (fireBallObject, [])
            (DirectionLight (Color3 1 0.9 0.8) (V3 (-1.86) 0.45 (-0.56)) 0.1)
            [("first-camera", Camera (V3 (-5) 0 (1.8 + 20)) 0 0 (pi/3))]
            0
    return world

disposeWorld :: World -> IO ()
disposeWorld world =
    mapM_ renderableDispose (fst (worldFireBalls world) : worldObjects world)

createContext :: IO Context
createContext = do
    screen <- createScreen
    deferredScreen <- createDeferredScreen
    let context = Context
            screen
            deferredScreen
            Nothing
            Nothing
            "first-camera"
            Set.empty
            (V2 0 0)
            Nothing
    return context

disposeContext :: Context -> IO ()
disposeContext context = do
    renderableDispose (contextScreen context)
    case contextShadowFrameBuffer context of
        Just (_, _, dispose) -> dispose
        Nothing -> return ()
    case contextGeometryFrameBuffer context of
        Just (_, _, dispose) -> dispose
        Nothing -> return ()

----------------------------------------------------------------------------------------------------

animate :: RenderingMode -> IORef World -> IORef Context -> Size -> Double -> IO Scene
animate renderingMode worldRef contextRef (Size width height) timeDelta = do
    world <- get worldRef
    context <- get contextRef

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
    worldRef $= world{ worldCameras = cameras }

    contextRef $= context{ contextDrag = Nothing }

    return $ createScene renderingMode worldRef contextRef

----------------------------------------------------------------------------------------------------

manipulate :: RenderingMode -> IORef World -> IORef Context -> Size -> Event -> IO (Maybe Scene)

-- Handle keyboard events.
manipulate renderingMode worldRef contextRef size (EventKey k _ ks _)

    -- Exit application on Escape.
    | k == GLFW.Key'Escape = return Nothing

    -- Dump the shadow map in a PNG on F1.
    | k == GLFW.Key'F1 && ks == GLFW.KeyState'Pressed = do
        context <- get contextRef
        case contextShadowFrameBuffer context of
            Just (_, depthTexture, _) -> saveDepthTexture (fromIntegral w, fromIntegral h) depthTexture "shadowmap.png"
                where Size w h = shadowMapSize
            Nothing -> return ()
        return $ Just (createScene renderingMode worldRef contextRef)

    -- Move the camera using WASD keys (note that the keyboard layout is not taken into account).
    | otherwise = do
        context <- get contextRef
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
    context <- get contextRef
    let drag = fromMaybe (V2 0 0) (contextDrag context) + V2 dx dy
    contextRef $= context{ contextDrag = Just drag }
    return $ Just (createScene renderingMode worldRef contextRef)

-- Shot randomly colored fire balls with the mouse right button.
manipulate renderingMode worldRef contextRef size (EventMouseButton b bs _) = do
    world <- get worldRef
    context <- get contextRef
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
            color <- runRandomIO $ Color3
                <$> getRandomR (0, 1)
                <*> getRandomR (0, 1)
                <*> getRandomR (0, 1)
            return [FireBall (cameraPosition camera) direction color 0]
        else return []
    let fireBalls' = second (++ newFireBalls) (worldFireBalls world)
    worldRef $= world{ worldFireBalls = fireBalls' }
    contextRef $= context
    return $ Just (createScene renderingMode worldRef contextRef)

-- Store the cursor location.
manipulate renderingMode worldRef contextRef size (EventCursorPos x y) = do
    world <- get worldRef
    context <- get contextRef
    contextRef $= context{ contextCursorPosition = V2 x y }
    return $ Just (createScene renderingMode worldRef contextRef)

-- Catch everything else.
manipulate renderingMode worldRef contextRef size _ =
    return $ Just (createScene renderingMode worldRef contextRef)

----------------------------------------------------------------------------------------------------

useShadowMap = True
shadowMapSize = Size 4096 4096

display :: RenderingMode -> IORef World -> IORef Context -> Size -> IO ()
display renderingMode worldRef contextRef size = do
    world <- get worldRef

    let completeRenderer (Renderable programs vao render dispose) (FireBall p d (Color3 cx cy cz) a) = do
            transformation <- newMatrix RowMajor (flattenMatrix (mkTransformation noRotation p)) :: IO (GLmatrix GLfloat)
            let render' p = do
                    setUniform p "transformation" transformation
                    setUniform p "emissiveColor" (Color4 cx cy cz 1.0)
                    render p
            return (Renderable programs vao render' dispose)
        (fireBallObject, fireBalls) = worldFireBalls world

    fireBallObjects <- mapM (completeRenderer fireBallObject) fireBalls

    let lights = map (\(FireBall p d c a) -> PointLight p c) fireBalls
        objects = worldObjects world ++ fireBallObjects
        elapsedTime = worldElapsedTime world

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
    context <- get contextRef

    let camera = fromJust (lookup (contextCameraName context) (worldCameras world))
        sun = worldSun world
        r = far / 8
        projectionMatrix = Linear.ortho (-r * 2) (r * 2) (-r) r (-r) r
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

    case contextShadowFrameBuffer context of
        Just (_, _, disposeFramebuffer) -> disposeFramebuffer
        Nothing -> return ()
    (fbo, texture, disposeFramebuffer) <- createShadowFrameBuffer size
    contextRef $= context{ contextShadowFrameBuffer = Just (fbo, texture, disposeFramebuffer) }

    backupViewport <- get viewport
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
    (Renderable programs vao render _)
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
    context <- get contextRef

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

    let isRenderableIn stage (Renderable programs vao render _) = isJust (lookup stage programs)
        renderObjectIn = renderObject
            size
            (worldElapsedTime world)
            shadowInfo
            camPosition
            projectionMat
            cameraMat
            (worldSun world)
            lights

    -- clearColor $= Color4 0.5 0.5 1.0 1.0
    -- clear [ColorBuffer, DepthBuffer]
    cullFace $= Just Back
    frontFace $= CW
    depthFunc $= Just Less
    -- clampColor ClampReadColor $= ClampOff

    case renderingMode of

        ForwardShading ->
            mapM_ (renderObjectIn ForwardShadingStage) objects

        DeferredShading -> do
            case contextGeometryFrameBuffer context of
                Just (_, _, disposeFramebuffer) -> disposeFramebuffer
                Nothing -> return ()
            (fbo, textures, disposeFramebuffer) <- createGeometryFrameBuffer size
            contextRef $= context{ contextGeometryFrameBuffer = Just (fbo, textures, disposeFramebuffer) }

            backupViewport <- get viewport
            viewport $= (Position 0 0, size)
            withDrawFramebuffer fbo $ do
                clearColor $= Color4 0 0 0 1
                depthFunc $= Just Less
                -- clampColor ClampReadColor $= ClampOff
                clear [ColorBuffer, DepthBuffer]
                mapM_ (renderObjectIn DeferredShadingStage) objects
            viewport $= backupViewport

            let (Renderable programs vao render dispose) = contextDeferredScreen context
                render' p = usingOrderedTextures p textures (render p)
            renderObjectIn ForwardShadingStage (Renderable programs vao render' dispose)

            {-
            In https://learnopengl.com/#!Advanced-Lighting/Deferred-Shading:
            What we need to do is first copy the depth information stored in the geometry pass into
            the default framebuffer's depth buffer and only then render the light cubes. This way
            the light cubes' fragments are only rendered when on top of the previously rendered
            geometry.
            -}
            bindFramebuffer ReadFramebuffer $= fbo
            clear [DepthBuffer]
            let (Position dx dy) = fst backupViewport
            blitFramebuffer
                (Position 0 0) (Position width heigh)
                (Position dx dy) (Position (dx + width) (dy + heigh))
                [DepthBuffer'] Nearest
            bindFramebuffer Framebuffer $= defaultFramebufferObject

            mapM_ (renderObjectIn ForwardShadingStage)
                (filter (not . isRenderableIn DeferredShadingStage) objects)

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
    (Renderable programs vao render _)
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

            setUniform program "fogColor" (Color4 (144/255) (142/255) (137/255) 1 :: Color4 GLfloat)
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
    context <- get contextRef

    let (Renderable programs vao render _) = contextScreen context

        defaultTexture = case contextShadowFrameBuffer context of
            Just (_, texture, _) -> Just texture
            Nothing -> Nothing

        pickTexture :: [TextureObject] -> Maybe TextureObject
        pickTexture textures = if index < length textures
            then Just (textures !! index)
            else defaultTexture

        renderTexture :: TextureObject -> IO ()
        renderTexture texture = case lookup ForwardShadingStage programs of
            Nothing -> return ()
            Just program -> withBinding currentExtProgram program .
                usingOrderedTextures program [texture] $
                    render program

    case contextGeometryFrameBuffer context of
        Just (_, textures, _) -> case pickTexture textures of
            Just texture -> renderTexture texture
            Nothing -> return ()
        Nothing -> case defaultTexture of
            Just texture -> renderTexture texture
            Nothing -> return ()

    printErrors "Errors"

