module Scene (
    Scene(..),
    RenderingMode(..),
    initWorld,
    initContext,
    initScene,
    initColorBufferScene
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
import Linear as LP
import Numeric (showGFloat)

import Buffer
import Debug
import Error
import Font
import FunctionalGL
import Heightmap
import Misc
import Random
import Shader
import Texture

import View
import World

----------------------------------------------------------------------------------------------------

data RenderingMode = ForwardShading | DeferredShading

shadowMapSize = (4096, 4096)

data Context = Context
    {   contextShadowContext :: (Context3D, TextureObject)
    ,   contextDeferredStage :: (Program, Dispose)
    ,   contextScreen :: Object3D
    ,   contextGeometryBuffer :: Maybe (FramebufferObject, [TextureObject], Dispose)
    ,   contextCameraName :: String
    ,   contextCameraMoves :: !(Set.Set Move)
    ,   contextCursorPosition :: V2 Double
    ,   contextDragMove :: Maybe (V2 Double)
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

getSight :: Camera -> V3 GLfloat
getSight camera = LP.rotate (axisAngle (getLeft camera) (cameraAltitude camera)) (getBearing camera)

getBearing :: Camera -> V3 GLfloat
getBearing camera = V3 (cos a) (sin a) 0 where a = cameraAzimuth camera

getLeft :: Camera -> V3 GLfloat
getLeft camera = V3 (cos a) (sin a) 0 where a = cameraAzimuth camera + pi / 2

getUp :: Camera -> V3 GLfloat
getUp camera = V3 0 0 1

data Scene = Scene
    { sceneRender :: Size -> IO ()
    , sceneAnimate :: Size -> Double -> IO Scene
    , sceneManipulate :: Size -> Event -> IO (Maybe Scene)
    }

-- Distinguer le monde partagé du monde UI subjectif (quoique partageable aussi).
initScene :: RenderingMode -> IORef World -> IORef Context -> Scene
initScene renderingMode worldRef contextRef = Scene
    (display renderingMode worldRef contextRef)
    (animate renderingMode worldRef contextRef)
    (manipulate renderingMode worldRef contextRef)

initColorBufferScene :: Int -> IORef Context -> Scene
initColorBufferScene index contextRef = Scene
    (displayBuffer contextRef index)
    (\_ _ -> return (initColorBufferScene index contextRef))
    (\_ _ -> return (Just (initColorBufferScene index contextRef)))

initWorld :: IO World
initWorld = do
    heightmap <- loadHeightmap "data/heightmap-257.png"
    let heightmapScale = V3 1 1 0.1
    terrain <- createRenderableTerrain heightmap heightmapScale
    objects <- sequence
        [ createSkyBox
        , return terrain
        -- , createNormalDisplaying heightmap terrain
        , createGrass heightmap heightmapScale
        , createSpaceInvader
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

initContext :: IO Context
initContext = do
    shadowContext <- createShadowContext shadowMapSize :: IO (Context3D, TextureObject)
    deferredStage <- createProgramWithShaders' "deferred_stage_vs.glsl" "deferred_stage_fs.glsl"
    screen <- createScreen
    let context = Context
            shadowContext
            deferredStage
            screen
            Nothing
            "first-camera"
            Set.empty
            (V2 0 0)
            Nothing
    return context

toWorld :: Size -> V2 Double -> M44 GLfloat -> M44 GLfloat -> V4 GLfloat
toWorld (Size width height) (V2 x y) projection camera = rectify target where
    (w, h) = (realToFrac width, realToFrac height)
    viewport = V4
        (V4 (w/2) 0 0 (w/2))
        (V4 0 (-h/2) 0 (h/2))
        (V4 0 0 (far - near) near)
        (V4 0 0 0 1)
    t = viewport !*! projection !*! camera
    target = inv44 t !* V4 (realToFrac x) (realToFrac y) 0 1
    rectify (V4 x y z w) = V4 (x / w) (y / w) (z / w) 1

-- TODO Preserve other cameras!
animate :: RenderingMode -> IORef World -> IORef Context -> Size -> Double -> IO Scene
animate renderingMode worldRef contextRef (Size width height) timeDelta = do
    world <- get worldRef
    context <- get contextRef
    let
        camera = fromJust (lookup (contextCameraName context) (worldCameras world))
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
        ((alt', az'), dragMove) = case contextDragMove context of
            Nothing -> ((alt, az), Nothing)
            Just from -> ((alt', az'), Just to) where
                to = contextCursorPosition context
                ratio = cameraFov camera / realToFrac height :: GLfloat
                V2 dAz dAlt = (realToFrac <$> (to - from)) * V2 ratio ratio
                alt' = alt + clamp (-pi/2.01-alt) (pi/2.01-alt) dAlt
                az' = mod' (az - dAz) (2 * pi)
        camera' = camera
            { cameraAltitude = alt'
            , cameraAzimuth = az'
            , cameraPosition = position }

    worldRef $= world
        { worldCameras = [(contextCameraName context, camera')]
        }

    contextRef $= context
        { contextDragMove = dragMove
        }

    return $ initScene renderingMode worldRef contextRef

manipulate :: RenderingMode -> IORef World -> IORef Context -> Size -> Event -> IO (Maybe Scene)

manipulate renderingMode worldRef contextRef size (EventKey k _ _ _) | k == GLFW.Key'Escape = do
    context <- get contextRef
    -- c3d_dispose (fst (contextShadowContext context))
    -- snd (contextDeferredStage context)
    return Nothing

manipulate renderingMode worldRef contextRef size (EventKey k _ ks _) | k == GLFW.Key'F1 && ks == GLFW.KeyState'Pressed = do
    context <- get contextRef
    let depthTexture = snd (contextShadowContext context)
    saveDepthTexture shadowMapSize depthTexture "shadowmap.png"
    return $ Just (initScene renderingMode worldRef contextRef)

manipulate renderingMode worldRef contextRef size event@(EventKey k _ ks _) = do
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
    return $ Just (initScene renderingMode worldRef contextRef)

manipulate renderingMode worldRef contextRef size (EventMouseButton b bs _) = do
    world <- get worldRef
    context <- get contextRef
    let dragMove = case b of
            GLFW.MouseButton'1 -> if bs == GLFW.MouseButtonState'Released
                then Nothing
                else Just (contextCursorPosition context)
            _ -> contextDragMove context
        camera = fromJust (lookup (contextCameraName context) (worldCameras world))
    newFireBalls <- if b == GLFW.MouseButton'2 && bs == GLFW.MouseButtonState'Pressed
        then do
            let (Size width height) = size
                projectionMatrix = LP.perspective (cameraFov camera) (width `divR` height) near far
                cameraMatrix = LP.lookAt
                    (cameraPosition camera)
                    (cameraPosition camera + getSight camera)
                    (getUp camera)
                cursor = contextCursorPosition context
                (V4 x y z _) = toWorld (Size width height) cursor projectionMatrix cameraMatrix
                direction = LP.normalize (V3 x y z - cameraPosition camera)
            color <- runRandomIO $ Color3
                <$> getRandomR (0, 1)
                <*> getRandomR (0, 1)
                <*> getRandomR (0, 1)
            return [FireBall (cameraPosition camera) direction color 0]
        else return []
    let fireBalls' = second (++ newFireBalls) (worldFireBalls world)
    worldRef $= world{ worldFireBalls = fireBalls' }
    contextRef $= context{ contextDragMove = dragMove }
    return $ Just (initScene renderingMode worldRef contextRef)

manipulate renderingMode worldRef contextRef size (EventCursorPos x y) = do
    world <- get worldRef
    context <- get contextRef
    contextRef $= context{ contextCursorPosition = V2 x y }
    return $ Just (initScene renderingMode worldRef contextRef)

manipulate renderingMode worldRef contextRef size _ =
    return $ Just (initScene renderingMode worldRef contextRef)

shadowStage :: IORef World -> IORef Context -> Size -> IO (GLmatrix GLfloat, TextureObject)
shadowStage worldRef contextRef size = do
    world <- get worldRef
    context <- get contextRef
    let (Context3D program render _, shadowMap) = contextShadowContext context
        camera = fromJust (lookup (contextCameraName context) (worldCameras world))
        sun = worldSun world
        r = far / 8
        projectionMatrix = LP.ortho (-r * 2) (r * 2) (-r) r (-r) r
        cameraMatrix = LP.lookAt
            (cameraPosition camera - directionLightDirection sun)
            (cameraPosition camera)
            (getUp camera)
            :: M44 GLfloat

    projectionMat <- newMatrix RowMajor (flattenMatrix projectionMatrix) :: IO (GLmatrix GLfloat)
    cameraMat <- newMatrix RowMajor (flattenMatrix cameraMatrix) :: IO (GLmatrix GLfloat)
    transformation <- newMatrix RowMajor (flattenMatrix identity) :: IO (GLmatrix GLfloat)

    withBinding currentProgram program $ do
        setUniform program "projection" projectionMat
        setUniform program "camera" cameraMat
        setUniform program "transformation" transformation
        render size program (worldObjects world)

    let biasMatrix = V4
            (V4 0.5 0.0 0.0 0.5)
            (V4 0.0 0.5 0.0 0.5)
            (V4 0.0 0.0 0.5 0.5)
            (V4 0.0 0.0 0.0 1.0)
        shadowMatrix = biasMatrix !*! projectionMatrix !*! cameraMatrix
    shadow <- newMatrix RowMajor (flattenMatrix shadowMatrix)

    return (shadow, shadowMap)

display :: RenderingMode -> IORef World -> IORef Context -> Size -> IO ()
display renderingMode worldRef contextRef size = do
    world <- get worldRef
    context <- get contextRef

    shadowInfo <- Just <$> shadowStage worldRef contextRef size
    -- let shadowInfo = Nothing

    -- clearColor $= Color4 0 0 0 1.0
    -- clear [ColorBuffer, DepthBuffer]
    cullFace $= Just Back
    frontFace $= CW
    depthFunc $= Just Less

    let (Size width height) = size
        camera = fromJust (lookup (contextCameraName context) (worldCameras world))
        camPosition = Vector3 cx cy cz where V3 cx cy cz = cameraPosition camera
        -- FOV (y direction, in radians), Aspect ratio, Near plane, Far plane
        projectionMatrix = LP.perspective (cameraFov camera) (width `divR` height) near far
        -- Eye, Center, Up
        cameraMatrix = LP.lookAt
            (cameraPosition camera)
            (cameraPosition camera + getSight camera)
            (getUp camera)

    projectionMat <- newMatrix RowMajor (flattenMatrix projectionMatrix)
    cameraMat <- newMatrix RowMajor (flattenMatrix cameraMatrix)

    let setLocation (Object3D program vao render dispose) (FireBall p d (Color3 cx cy cz) a) = do
            transformation <- newMatrix RowMajor (flattenMatrix (mkTransformation noRotation p)) :: IO (GLmatrix GLfloat)
            let render' s p = do
                    setUniform p "transformation" transformation
                    setUniform p "emissiveColor" (Color4 cx cy cz 1.0)
                    render s p
            return (Object3D program vao render' dispose)

    let (fireBallObject, fireBalls) = worldFireBalls world
    fireBallObjects <- mapM (setLocation fireBallObject) fireBalls
    let toLight (FireBall p d c a) = PointLight p c
        elapsedTime = worldElapsedTime world
        renderObject = renderObjectInForwardShading
            size
            elapsedTime
            shadowInfo
            camPosition
            projectionMat
            cameraMat
            (worldSun world)

    case renderingMode of
        ForwardShading ->
            mapM_ (renderObject (map toLight fireBalls)) (worldObjects world ++ fireBallObjects)
        DeferredShading -> do
            -- In fragment shaders:
            -- layout (location = 0) out vec3 position;
            -- layout (location = 1) out vec3 normal;
            -- layout (location = 2) out vec4 albedoAndSpecular;
            let (deferredProgram, _) = contextDeferredStage context

            case contextGeometryBuffer context of
                Just (_, _, disposeFramebuffer) -> disposeFramebuffer
                Nothing -> return ()
            (fbo, textures, disposeFramebuffer) <- createGeometryBuffer (fromIntegral width, fromIntegral height)
            contextRef $= context{ contextGeometryBuffer = Just (fbo, textures, disposeFramebuffer) }

            bindFramebuffer DrawFramebuffer $= fbo

            -- viewport $= (Position 0 0, size)
            clear [ColorBuffer, DepthBuffer] -- +
            {-
            cullFace $= Nothing
            depthFunc $= Just Less
            frontFace $= CW
            -}
            transformation <- newMatrix RowMajor (flattenMatrix identity) :: IO (GLmatrix GLfloat)

            withBinding currentProgram deferredProgram $ do
                setUniform deferredProgram "projection" projectionMat
                setUniform deferredProgram "camera" cameraMat
                setUniform deferredProgram "transformation" transformation
                forM_ (worldObjects world ++ fireBallObjects) $
                    \(Object3D _ vao render _) -> withBinding bindVertexArrayObject vao $
                        render size deferredProgram

            bindFramebuffer DrawFramebuffer $= defaultFramebufferObject

            mapM_ (renderObject (map toLight fireBalls)) (worldObjects world ++ fireBallObjects)

    let cursor = contextCursorPosition context
        target = toWorld size cursor projectionMatrix cameraMatrix

    printErrors "Errors"
    -- displayText size (stateFont state) (showV2 cursor ++ " -> " ++ showV4 target)

displayBuffer :: IORef Context -> Int -> Size -> IO ()
displayBuffer contextRef index size = do
    context <- get contextRef

    let (Object3D program vao render _) = contextScreen context
        (_, texture) = contextShadowContext context
        pickTexture textures = if index < length textures
            then textures !! index
            else texture

    case contextGeometryBuffer context of
        Just (_, textures, _) ->
            withBinding currentProgram program .
            withBinding bindVertexArrayObject vao .
            usingOrderedTextures program [pickTexture textures] $
                render size program
        Nothing ->
            withBinding currentProgram program .
            withBinding bindVertexArrayObject vao .
            usingOrderedTextures program [texture] $
                render size program

    printErrors "Errors"

renderObjectInForwardShading
    :: Size
    -> Double
    -> Maybe (GLmatrix GLfloat, TextureObject)
    -> Vector3 GLfloat
    -> GLmatrix GLfloat
    -> GLmatrix GLfloat
    -> DirectionLight
    -> [PointLight]
    -> Object3D
    -> IO ()
renderObjectInForwardShading
    size
    elapsedTime
    shadowInfo
    cameraPosition
    projection
    camera
    sun
    lights
    (Object3D program vao render _)
    =
    withBinding bindVertexArrayObject vao . withBinding currentProgram program $ do

        setUniform program "cameraPosition" cameraPosition

        setUniform program "projection" projection
        setUniform program "camera" camera
        transformation <- newMatrix RowMajor (flattenMatrix identity) :: IO (GLmatrix GLfloat)
        setUniform program "transformation" transformation

        setUniform program "emissiveColor" (Color4 1 1 1 1 :: Color4 GLfloat)

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

        setUniform program "materialSpecularIntensity" (5 :: GLfloat)
        setUniform program "materialSpecularPower" (20 :: GLfloat)

        setUniform program "timePassed" (F.double2Float elapsedTime :: GLfloat)

        let contextualizeAction = case shadowInfo of
                Just (shadow, shadowMap) -> \action -> do
                    let shadowSampler = TextureUnit 8
                    activeTexture $= shadowSampler
                    textureBinding Texture2D $= Just shadowMap
                    setUniform program "shadowMap" shadowSampler
                    setUniform program "shadow" shadow
                    action
                    activeTexture $= shadowSampler
                    textureBinding Texture2D $= Nothing
                Nothing -> id

        contextualizeAction (render size program)

----------------------------------------------------------------------------------------------------

createShadowContext :: (Int, Int) -> IO (Context3D, TextureObject)
createShadowContext (width, height) = do
    (fbo, depthTexture, disposeFramebuffer) <- createShadowBuffer (width, height)
    -- In fragment shaders:
    -- layout(location = 0) out vec3 color;
    (program, disposeProgram) <- createProgramWithShaders' "shadow_vs.glsl" "shadow_fs.glsl"

    let size = Size (fromIntegral width) (fromIntegral height)
        render realSize program objects = do
            backupViewport <- get viewport
            viewport $= (Position 0 0, size)
            bindFramebuffer DrawFramebuffer $= fbo
            clear [DepthBuffer]
            cullFace $= Nothing
            frontFace $= CW
            forM_ objects $ \(Object3D _ vao render _) ->
                withBinding bindVertexArrayObject vao $
                render size program
            viewport $= backupViewport
            bindFramebuffer DrawFramebuffer $= defaultFramebufferObject

    return (Context3D program render (disposeProgram >> disposeFramebuffer), depthTexture)
