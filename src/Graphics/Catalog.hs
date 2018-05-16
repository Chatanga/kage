module Graphics.Catalog (
    createSpaceInvader,
    createFireBall,
    createVectorDisplaying,
    createRenderableTerrain,
    createHeightmapNormalDisplaying,
    createNormalDisplaying,
    createGrass,
    createRenderableBoxSet,
    createSolidBox,
    createTransparentBox,
    createSkyBox,
    createGrid,
    createTesselatedPyramid,
    --
    createScreen,
    createSimpleScreen,
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

import Common.Debug
import Common.Misc

import Graphics.Buffer
import Graphics.Font
import Graphics.FunctionalGL as FGL
import Graphics.Geometry
import Graphics.Heightmap
import Graphics.Shader
import Graphics.Texture

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
    return (Renderable [(DirectShadingStage, program)] vao render' (liftIO disposeSphere >> releaseProgram) translucentZOrder)

createVectorDisplaying :: V3 Float -> Color4 Float -> ResourceIO Renderable
createVectorDisplaying v color = do
    (vao, render, disposeVector) <- liftIO $ createVector v
    (program, releaseProgram) <- acquireProgramWithShaders
        [ ("vector_vs.glsl", VertexShader)
        , ("vector_gs.glsl", GeometryShader)
        , ("vector_fs.glsl", FragmentShader)
        ]
    let render' p = do
            setUniform p "color" color
            render p
    return (Renderable [(DirectShadingStage, program)] vao render' (liftIO disposeVector >> releaseProgram) defaultZOrder)

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
            , (DirectShadingStage, program2)
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

    return (Renderable programs vao render' releaseAll terrainZOrder)

createHeightmapNormalDisplaying :: Heightmap Float -> Renderable -> ResourceIO Renderable
createHeightmapNormalDisplaying (w, h, _) = createNormalDisplaying (fromIntegral (w * h))

createNormalDisplaying :: Int -> Renderable -> ResourceIO Renderable
createNormalDisplaying pointCount (Renderable _ vao _ _ _) = do
    let render p = withBinding bindVertexArrayObject vao $ drawArrays Points 0 (fromIntegral pointCount)
    (program, releaseProgram) <- acquireProgramWithShaders
        [ ("normal_vs.glsl", VertexShader)
        , ("normal_gs.glsl", GeometryShader)
        , ("normal_fs.glsl", FragmentShader)
        ]
    return (Renderable [(DirectShadingStage, program)] vao render releaseProgram defaultZOrder)

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

    return (Renderable [(DirectShadingStage, program)] vao render' releaseAll 200)

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
            , (DirectShadingStage, program2)
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

    return (Renderable programs vao render' releaseAll defaultZOrder)

createSolidBox :: Float -> ResourceIO Renderable
createSolidBox edgeSize = do
    (vao, render, disposeBox) <- liftIO $ createBox edgeSize
    (program, releaseProgram) <- acquireProgramWithShaders' "generic_vs.glsl" "solid_box_fs.glsl"

    let render' p = do
            setUniform p "materialSpecularIntensity" (5 :: GLfloat)
            setUniform p "materialSpecularPower" (20 :: GLfloat)
            render p

    return (Renderable [(DirectShadingStage, program)] vao render' (liftIO disposeBox >> releaseProgram) defaultZOrder)

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

    return (Renderable [(DirectShadingStage, program)] vao render' (liftIO disposeBox >> releaseProgram) translucentZOrder)

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

    return (Renderable [(DirectShadingStage, program)] vao render' releaseAll skyBoxZOrder)

createGrid :: ResourceIO Renderable
createGrid = do
    (vao, render, disposeSquare) <- liftIO $ createSquare (0, 0) 100
    (program, releaseProgram) <- acquireProgramWithShaders' "grid_vs.glsl" "grid_fs.glsl"

    let render' =
            FGL.withState blend Enabled .
            FGL.withState blendFunc (SrcAlpha, OneMinusSrcAlpha) .
            FGL.withState depthMask Disabled .
            FGL.withState cullFace Nothing .
            render

    return (Renderable [(DirectShadingStage, program)] vao render' (liftIO disposeSquare >> releaseProgram) translucentZOrder)

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

    return (Renderable [(DirectShadingStage, program)] vao render' (liftIO dispose >> acquireProgram) defaultZOrder)

----------------------------------------------------------------------------------------------------

createScreen :: ResourceIO Renderable
createScreen = _createScreen ""

createSimpleScreen :: ResourceIO Renderable
createSimpleScreen = _createScreen "_simple"

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
    return (Renderable [(DirectShadingStage, program)] vao render (liftIO disposeSquare >> releaseProgram) defaultZOrder)
