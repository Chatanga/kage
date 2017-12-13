module World (
    FireBall(..),
    DirectionLight(..),
    PointLight(..),
    World(..),
    animateWorld,
    --
    createSpaceInvader,
    createFireBall,
    createRenderableTerrain,
    createHeightmapNormalDisplaying,
    createNormalDisplaying,
    createGrass,
    createRenderableBoxSet,
    createTransparentBox,
    createSkyBox,
    createTesselatedPyramid,
    --
    createScreen,
    createSsaoScreen,
    createBlurScreen,
    createLightingScreen,
    createToneMappingScreen,
    createColorBlurScreen,
    createColorCombineScreen
) where

import Control.Arrow
import Control.Monad
import Control.Monad.State as S
import Data.List
import Data.Maybe
import qualified Data.Set as Set

import Graphics.Rendering.OpenGL
import Linear
import Numeric

import Buffer
import FunctionalGL as FGL
import Geometry
import Heightmap
import Misc
import Shader
import Texture
import Debug

----------------------------------------------------------------------------------------------------

{-
    Inclure à la fois l’aspect physique, sonore et 3D.
    Une sorte de modèle de présentation donc, partagé,
    mais orienté vue.
-}

----------------------------------------------------------------------------------------------------

data FireBall = FireBall
    {   fireBallPosition :: !(V3 GLfloat)
    ,   fireBallDirection :: !(V3 GLfloat)
    ,   fireBallColor :: !(Color3 GLfloat)
    ,   fireBallAge :: !Double
    }   deriving (Show)

data DirectionLight = DirectionLight
    {   directionLightColor :: !(Color3 GLfloat)
    ,   directionLightDirection :: !(V3 GLfloat)
    ,   directionLightAmbientIntensity :: !GLfloat
    }   deriving (Show)

data PointLight = PointLight
    {   pointLightPosition :: !(V3 GLfloat)
    ,   pointLightColor :: !(Color3 GLfloat)
    }   deriving (Show)

data AnyValue = AnyBoolean Bool | AnyString String | AnyInt Int | AnyFloat Float

data Object = Object
    {   objectTransformation :: !(Linear.M44 Float)
    ,   objectBoundingInfo :: !(Maybe BoundingInfo)
    ,   objectOptions :: [(String, AnyValue)] -- [(name, value)]
    ,   objectRender :: Program -> IO ()
    ,   objectDispose :: IO ()
    -- Ajouter un aspect physique.
    }

instance Show Object where
    show _ = "<Object>"

data World = World
    {   worldTerrain :: !(Heightmap Float, V3 Float)
    ,   worldObjects :: ![Renderable] -- TODO as a tree ?
    ,   worldFireBalls :: !(Renderable, [FireBall])
    ,   worldSun :: !DirectionLight
    ,   worldCameras :: ![(String, Camera)]
    ,   worldElapsedTime :: !Double
    }

type DurationInMs = Double

animateWorld :: DurationInMs -> World -> ResourceIO World
animateWorld timeDelta world = return world' where

    fireBalls = second (mapMaybe updateFireBall) (worldFireBalls world)
    updateFireBall (FireBall p d c a)
        | a < 10 = Just (FireBall (p + d/2) d c (a + timeDelta))
        | otherwise = Nothing

    sun = worldSun world
    sun' = sun
        { directionLightDirection = Linear.rotate
            (axisAngle (V3 0 0 1) (realToFrac timeDelta / 4))
            (directionLightDirection sun)
        }

    elapsedTime = worldElapsedTime world + timeDelta

    world' = world
        { worldFireBalls = fireBalls
        , worldSun = sun'
        , worldElapsedTime = elapsedTime
        }

----------------------------------------------------------------------------------------------------

alien =
    [ "░░█░░░░░█░░"
    , "░░░█░░░█░░░"
    , "░░███████░░"
    , "░██░███░██░"
    , "███████████"
    , "█░███████░█"
    , "█░█░░░░░█░█"
    , "░░░██░██░░░"
    ]

createSpaceInvader :: V3 Float -> ResourceIO Renderable
createSpaceInvader position = createRenderableBoxSet 2 $ map ((+) position . (*) 2) offsets where
    rowCount = fromIntegral (length alien)
    offsets = catMaybes . concat $
        for (zip [0..] alien) $ \(y, row) ->
            for (zip [0..] row) $ \(x, c) ->
                if c == '█' then Just (V3 0 x (rowCount - y)) else Nothing

createFireBall :: ResourceIO Renderable
createFireBall = do
    (vao, render, disposeSphere) <- liftIO $ createSphere 0
    (program, releaseProgram) <- acquireProgramWithShaders
        [ ("fireball_vs.glsl", VertexShader)
        , ("fireball_gs.glsl", GeometryShader)
        , ("fireball_fs.glsl", FragmentShader)
        ]
    let render' p = FGL.withState blend Enabled .
            FGL.withState depthMask Disabled .
            FGL.withState blendFunc (SrcAlpha, OneMinusSrcAlpha) .
            FGL.withState cullFace (Just Back) $ render p
    return (Renderable [(ForwardShadingStage, program)] vao render' (liftIO disposeSphere >> releaseProgram))

createRenderableTerrain :: Heightmap Float -> V3 Float -> ResourceIO Renderable
createRenderableTerrain heightmap scale = do
    (vao, render, disposeTerrain) <- liftIO $ createTerrain heightmap scale
    (textures, releaseTextures) <- unzip <$>
        mapMaybeM acquireImage
            [ "data/ground_1.png"
            , "data/ground_5.png"
            , "data/ground_3.png"
            ]
    (program1, releaseProgram1) <- acquireProgramWithShaders' "shadow_vs.glsl" "shadow_fs.glsl"
    (program2, releaseProgram2) <- acquireProgramWithShaders' "generic_vs.glsl" "terrain_fs.glsl"
    (program3, releaseProgram3) <- acquireProgramWithShaders' "generic_vs.glsl" "terrain_deferred_fs.glsl"

    let programs =
            [ (ShadowMappingStage, program1)
            , (ForwardShadingStage, program2)
            , (DeferredShadingStage, program3)
            ]
        releaseAll = do
            liftIO disposeTerrain
            releaseProgram1
            releaseProgram2
            releaseProgram3
            sequence_ releaseTextures

    let render' p =
            {- Ne sert à rien et pose problème en rendu différé.
            withState blend Enabled
            withState blendFunc (SrcAlpha, OneMinusSrcAlpha)
            -}
            usingOrderedTextures p textures $ render p

    return (Renderable programs vao render' releaseAll)

createHeightmapNormalDisplaying :: Heightmap Float -> Renderable -> ResourceIO Renderable
createHeightmapNormalDisplaying (w, h, _) = createNormalDisplaying (fromIntegral (w * h))

createNormalDisplaying :: Int -> Renderable -> ResourceIO Renderable
createNormalDisplaying pointCount (Renderable _ vao _ _) = do
    let render p = withBinding bindVertexArrayObject vao $ drawArrays Points 0 (fromIntegral pointCount)
    (program, releaseProgram) <- acquireProgramWithShaders
        [ ("normal_vs.glsl", VertexShader)
        , ("normal_gs.glsl", GeometryShader)
        , ("normal_fs.glsl", FragmentShader)
        ]
    return (Renderable [(ForwardShadingStage, program)] vao render releaseProgram)

createGrass :: Heightmap Float -> V3 Float -> ResourceIO Renderable
createGrass heightmap scale = do
    (vao, render, disposeTerrain) <- liftIO $ createRandomMesh heightmap scale
    let configureTexture t = withTexture2D t $ do
            forM_ [S, T] $ \coord -> textureWrapMode Texture2D coord $= (Mirrored, Repeat)
            return t
    (preTextures, releaseTextures) <- unzip <$> mapMaybeM acquireImage
        [ "data/grass_02/grass_04/diffus.tga"
        , "data/grass_02/grass_05/diffus.tga"
        , "data/grass_02/grass_06/diffus.tga"
        ]
    textures <- liftIO $ mapM configureTexture preTextures
    (program, releaseProgram) <- acquireProgramWithShaders
        [ ("grass_vs.glsl", VertexShader)
        , ("grass_gs.glsl", GeometryShader)
        , ("grass_fs.glsl", FragmentShader)
        ]

    let render' p =
            -- withState blend Enabled .
            -- withState blendFunc (SrcAlpha, OneMinusSrcAlpha) .
            FGL.withState cullFace Nothing .
            usingOrderedTextures p textures $ do
            setUniform p "grassColor" (Color4 1 1 1 1 :: Color4 GLfloat)
            setUniform p "grassAlphaTest" (0.2 :: GLfloat)
            setUniform p "grassAlphaMultiplier" (1.2 :: GLfloat)
            render p
    let releaseAll = do
            liftIO disposeTerrain
            releaseProgram
            sequence_ releaseTextures

    return (Renderable [(ForwardShadingStage, program)] vao render' releaseAll)

createRenderableBoxSet :: Float -> [V3 GLfloat] -> ResourceIO Renderable
createRenderableBoxSet edgeSize translations = do
    (vao, render, disposeBox) <- liftIO $ createTexturedBox edgeSize

    (textures, disposeTextures) <- unzip <$>
        mapMaybeM acquireImage
            [ "data/RustMixedOnPaint012_1k/RustMixedOnPaint012_COL_VAR1_1K.jpg"
            , "data/RustMixedOnPaint012_1k/RustMixedOnPaint012_NRM_1K.jpg"
            , "data/RustMixedOnPaint012_1k/RustMixedOnPaint012_REFL_1K.jpg"
            , "data/RustMixedOnPaint012_1k/RustMixedOnPaint012_GLOSS_1K.jpg"
            ]
        {-
            [ "data/Bricks01_1k/Bricks01_COL_VAR1_1K.jpg"
            , "data/Bricks01_1k/Bricks01_NRM_1K.jpg"
            , "data/Bricks01_1k/Bricks01_REFL_1K.jpg"
            , "data/Bricks01_1k/Bricks01_GLOSS_1K.jpg"
            ]
        -}

    (program1, disposeProgram1) <- acquireProgramWithShaders' "shadow_vs.glsl" "shadow_fs.glsl"
    (program2, disposeProgram2) <- acquireProgramWithShaders' "box_vs.glsl" "box_fs.glsl"
    (program3, disposeProgram3) <- acquireProgramWithShaders' "box_vs.glsl" "box_deferred_fs.glsl"

    let programs =
            [ (ShadowMappingStage, program1)
            , (ForwardShadingStage, program2)
            , (DeferredShadingStage, program3)
            ]
        releaseAll = do
            liftIO disposeBox
            disposeProgram1
            disposeProgram2
            disposeProgram3
            sequence_ disposeTextures

    transformations <- liftIO $ mapM
        (newMatrix RowMajor . flattenMatrix . mkTransformation noRotation) translations
        :: ResourceIO [GLmatrix GLfloat]
    -- TODO Use instanciation instead.
    let render' p = forM_ transformations $ \transformation -> do
            setUniform p "transformation" transformation
            setUniform p "materialSpecularIntensity" (5 :: GLfloat)
            setUniform p "materialSpecularPower" (20 :: GLfloat)
            usingOrderedTextures p textures (render p)

    return (Renderable programs vao render' releaseAll)

createTransparentBox :: Float -> ResourceIO Renderable
createTransparentBox edgeSize = do
    (vao, render, disposeBox) <- liftIO $ createBox edgeSize
    (program, releaseProgram) <- acquireProgramWithShaders' "generic_vs.glsl" "box_fs.glsl"

    let -- rotation = axisAngle (V3 1 2 3) (pi / 5)
        translation = V3 (-1.5) (-0.5) 0.6
        t = mkTransformation noRotation translation

    transformation <- liftIO $ newMatrix RowMajor (flattenMatrix t) :: ResourceIO (GLmatrix GLfloat)

    let render' p = do
            setUniform p "transformation" transformation
            FGL.withState blend Enabled .
                FGL.withState blendFunc (SrcAlpha, OneMinusSrcAlpha) .
                FGL.withState depthMask Disabled .
                FGL.withState cullFace Nothing $ render p

    return (Renderable [(ForwardShadingStage, program)] vao render' (liftIO disposeBox >> releaseProgram))

createSkyBox :: ResourceIO Renderable
createSkyBox = do
    (vao, render, disposeSkyBox) <- liftIO $ createTexturedSkyBox far -- Using far is cheating here...
    let cloudTextureNames = map
            (\ext -> "data/skybox/cloudtop/cloudtop_" ++ ext ++ ".tga")
            ["dn", "up", "lf", "bk", "rt", "ft"]
        elbrusTextureNames = map
            (\ext -> "data/skybox/elbrus/elbrus_" ++ ext ++ ".jpg")
            ["top", "top", "right", "back", "left", "front"]
        configureTexture t = withTexture2D t $ do
            textureWrapMode Texture2D S $= (Mirrored, Repeat)
            textureWrapMode Texture2D T $= (Mirrored, Repeat)
            return t
    (preTextures, releaseTextures) <- unzip <$> mapMaybeM acquireImage cloudTextureNames
    textures <- liftIO $ mapM configureTexture preTextures

    (program, releaseProgram) <- acquireProgramWithShaders' "skybox_vs.glsl" "skybox_fs.glsl"

    let render' p =
            FGL.withState cullFace (Just Front) .
            FGL.withState depthMask Disabled .
            usingOrderedTextures program textures $ render p
        releaseAll = do
            liftIO disposeSkyBox
            releaseProgram
            sequence_ releaseTextures

    return (Renderable [(ForwardShadingStage, program)] vao render' releaseAll)

createTesselatedPyramid :: ResourceIO Renderable
createTesselatedPyramid = do
    (vao, render, dispose) <- liftIO createPatchPyramid
    (program, acquireProgram) <- acquireProgramWithShaders
        [ ("triangle_vs.glsl", VertexShader)
        , ("triangle_cs.glsl", TessControlShader)
        , ("triangle_es.glsl", TessEvaluationShader)
        , ("triangle_fs.glsl", FragmentShader)
        ]

    let render' p = do
            patchVertices $= 3
            FGL.withState polygonMode (Line, Line) .
                FGL.withState cullFace Nothing $
                render p

    return (Renderable [(ForwardShadingStage, program)] vao render' (liftIO dispose >> acquireProgram))

----------------------------------------------------------------------------------------------------

createScreen :: ResourceIO Renderable
createScreen = _createScreen ""

createSsaoScreen :: ResourceIO Renderable
createSsaoScreen = _createScreen "_ssao"

createBlurScreen :: ResourceIO Renderable
createBlurScreen = _createScreen "_blur"

createLightingScreen :: ResourceIO Renderable
createLightingScreen = _createScreen "_lighting"

createToneMappingScreen :: ResourceIO Renderable
createToneMappingScreen = _createScreen "_tone-mapping"

createColorBlurScreen :: ResourceIO Renderable
createColorBlurScreen = _createScreen "_color-blur"

createColorCombineScreen :: ResourceIO Renderable
createColorCombineScreen = _createScreen "_color-combine"

_createScreen :: String -> ResourceIO Renderable
_createScreen suffix = do
    (vao, render, disposeSquare) <- liftIO $ createSquare (0, 0) 2
    (program, releaseProgram) <- acquireProgramWithShaders' "screen_vs.glsl" ("screen"++ suffix ++"_fs.glsl")
    return (Renderable [(ForwardShadingStage, program)] vao render (liftIO disposeSquare >> releaseProgram))
