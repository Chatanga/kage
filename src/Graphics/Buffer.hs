{-# LANGUAGE FlexibleInstances #-}

module Graphics.Buffer
    (   createSphere
    ,   createSquare
    ,   createVector
    ,   createIndexedPyramid
    ,   createPatchPyramid
    ,   createBox
    ,   createTexturedBox
    ,   createTexturedSkyBox
    ,   createTexturedMesh
    ,   createRandomMesh
    ,   createTerrain130
    ,   createTerrain
    ,   createFlatTerrain
    ,   createShadowFrameBuffer
    ,   createGeometryFrameBuffer
    ,   createSimpleFrameBuffer
    ,   createHdrFrameBuffer
    ,   createBloomFrameBuffer
    ,   createColorFrameBuffer
    ,   flattenMatrix
    ,   flattenVertices
        ---
    ,   createMultiplexedObject
    ,   createIndexedMultiplexedObject
    ,   createMultiplexedVbo
    ,   createObject
    ,   createIbo
    ) where

import Control.Monad
import Data.Function
import Data.Maybe
import Data.Tuple (swap)
import qualified Data.Vector.Storable as V
import Foreign (Ptr, plusPtr, castPtr, nullPtr, sizeOf, with)
import Graphics.Rendering.OpenGL
import Linear as LP

import Codec.Picture
import Data.Vector.Storable (unsafeWith)
import System.Log.Logger

import Common.Misc
import Common.Random
import Common.Debug

import Graphics.Error
import Graphics.FunctionalGL
import Graphics.Heightmap
import Graphics.Texture

----------------------------------------------------------------------------------------------------

{-
GLSL: layout(location = 0) in vec3 vertexPosition;
or
Haskell: attribLocation program "vertexPosition" $= AttribLocation 0
-}
pointLocation = 0
colorLocation = 1
texCoordLocation = 2
normalLocation = 3
tangentLocation = 4

createSphere :: Int -> IO (VertexArrayObject, Render, Dispose)
createSphere depth = do
    let x = 0.525731112119133606
        z = 0.850650808352039932

        vertices :: [V3 GLfloat]
        vertices = map (\(x, y, z) -> V3 x y z)
            [ (-x, 0, z), (x, 0, z), (-x, 0, -z), (x, 0, -z)
            , (0, z, x), (0, z, -x), (0, -z, x), (0, -z, -x)
            , (z, x, 0), (-z, x, 0), (z, -x, 0), (-z, -x , 0)
            ]

        indices :: [(Int, Int, Int)]
        indices =
            [ (1, 4, 0), (4, 9, 0), (4, 5, 9), (8, 5, 4), (1, 8, 4)
            , (1, 10, 8), (10, 3, 8), (8, 3, 5), (3, 2, 5), (3, 7, 2)
            , (3, 10, 7), (10, 6, 7), (6, 11, 7), (6, 0, 11), (6, 1, 0)
            , (10, 1, 6), (11, 0, 9), (2, 11, 9), (5, 2, 9), (11, 2, 7)
            ]

        divide :: Int -> (V3 GLfloat, V3 GLfloat, V3 GLfloat) -> [V3 GLfloat]
        divide 0 (p1, p2, p3) = map LP.normalize [p1, p2, p3]
        divide depth (p_1, p_2, p_3) =
            let p12 = p_1 + p_2
                p23 = p_2 + p_3
                p31 = p_3 + p_1
            in  concatMap (divide (depth - 1))
                [ (p_1, p12, p31)
                , (p12, p_2, p23)
                , (p31, p23, p_3)
                , (p12, p23, p31)
                ]

        getTriangle (i, j, k) = (vertices !! i, vertices !! k, vertices !! j)
        points = flattenVertices $ concatMap (divide depth . getTriangle) indices

    createMultiplexedObject (points, [(pointLocation, 3)]) (drawArrays Triangles)

createSquare :: (Float, Float) -> Float -> IO (VertexArrayObject, Render, Dispose)
createSquare (dx, dy) s = createObject Triangles
    -- Points
    (Just $ zipWith (+) (cycle [dx, dy, 0]) $ map (* s)
            [ -0.5, 0.5, 0
            , 0.5, -0.5, 0
            , -0.5, -0.5, 0
            , -0.5, 0.5, 0
            , 0.5, 0.5, 0
            , 0.5, -0.5, 0
            ])
    -- Colors
    Nothing
    -- Texture coordinates
    (Just
        [ 0, 1
        , 1, 0
        , 0, 0
        , 0, 1
        , 1, 1
        , 1, 0
        ])
    -- Normals
    Nothing
    -- Tangents
    Nothing
    -- Indices
    Nothing

createVector :: V3 GLfloat -> IO (VertexArrayObject, Render, Dispose)
createVector (V3 x y z) =
    createMultiplexedObject ([x, y, z], [(pointLocation, 3)]) (drawArrays Points)

createIndexedPyramid :: IO (VertexArrayObject, Render, Dispose)
createIndexedPyramid = createObject Triangles
    -- Points
    (Just
        [ -5, 0, -3
        , -5, 0, 3
        , 5, 0, 3
        , 5, 0, -3
        , 0, 5, 0
        ])
    -- Colors
    (Just
        [ 0, 0, 1
        , 0, 1, 0
        , 0, 0, 1
        , 0, 1, 0
        , 1, 0, 0
        ])
    -- Texture coordinates
    Nothing
    -- Normals
    Nothing
    -- Tangents
    Nothing
    -- Indices
    (Just
        [ 0, 1, 2
        , 0, 2, 3
        , 4, 2, 1
        , 4, 3, 2
        , 4, 0, 3
        , 4, 1, 0
        ])

createPatchPyramid :: IO (VertexArrayObject, Render, Dispose)
createPatchPyramid = createObject Patches
        -- Points
        (Just $ concatMap (\i -> map (\n -> points !! (i * 3 + n)) [0, 1, 2]) indices)
        -- Colors
        Nothing
        -- Texture coordinates
        Nothing
        -- Normals
        Nothing
        -- Tangents
        Nothing
        -- Indices
        Nothing
    where
        points =
            [ -5, 0, -3
            , -5, 0, 3
            , 5, 0, 3
            , 5, 0, -3
            , 0, 5, 0
            ]
        indices =
            [ 0, 1, 2
            , 0, 2, 3
            , 4, 2, 1
            , 4, 3, 2
            , 4, 0, 3
            , 4, 1, 0
            ]

createBaseBox :: GLfloat -> [GLfloat]
createBaseBox edgeSize =
    let a = edgeSize / 2
        z = -a
    in  [ z, z, z
        , a, z, z
        , z, a, z
        , a, a, z
        , z, z, a
        , a, z, a
        , z, a, a
        , a, a, a
        ]

createBox :: GLfloat -> IO (VertexArrayObject, Render, Dispose)
createBox edgeSize = do
    let points = createBaseBox edgeSize
        colors = concatMap (take (12 * 3) . cycle)
            [   [1, 0, 0]
            ,   [0, 1, 0]
            ,   [1, 1, 0]
            ,   [0, 0, 1]
            ,   [1, 0, 1]
            ,   [0, 1, 1]
            ]
        triangles = concatMap (\i -> V.toList (V.slice (i * 3) 3 (V.fromList points)))
            [ 1, 3, 2
            , 1, 2, 0
            , 4, 6, 7
            , 4, 7, 5
            , 0, 4, 5
            , 0, 5, 1
            , 1, 5, 7
            , 1, 7, 3
            , 3, 7, 6
            , 3, 6, 2
            , 2, 6, 4
            , 2, 4, 0
            ]
        normals = flattenVertices (extractQuadNormals (unflattenVertices3 triangles))
    createObject Triangles (Just triangles) (Just colors) Nothing (Just normals) Nothing Nothing

createTexturedBox :: GLfloat -> IO (VertexArrayObject, Render, Dispose)
createTexturedBox edgeSize = do
    let points = createBaseBox edgeSize
        texCoords = (concat . replicate 6)
            [ 0, 1
            , 0, 0
            , 1, 0
            , 0, 1
            , 1, 0
            , 1, 1
            ]
        triangles = concatMap (\i -> let i3 = i*3 in [points !! i3, points !! (i3+1), points !! (i3+2)])
            [ 1, 3, 2
            , 1, 2, 0
            , 4, 6, 7
            , 4, 7, 5
            , 0, 4, 5
            , 0, 5, 1
            , 1, 5, 7
            , 1, 7, 3
            , 3, 7, 6
            , 3, 6, 2
            , 2, 6, 4
            , 2, 4, 0
            ]
        normals = flattenVertices (extractQuadNormals (unflattenVertices3 triangles))
        tangents = flattenVertices (extractQuadTangents (unflattenVertices3 triangles))
    createObject Triangles (Just triangles) Nothing (Just texCoords) (Just normals) (Just tangents) Nothing

{-
   (V)

    ^
    | p1'   p3'
 p1 +------+
    |\     |
    | \    |      n is pointing out
    |  \   |        of the screen
  b |   \  |
    |    \ |
    |     \| p2'
  --+------+---------> (U)
 p2 |  t   p3

-}
extractQuadNormals :: [V3 GLfloat] -> [V3 GLfloat]
extractQuadNormals [] = []
extractQuadNormals (p1 : p2 : p3 : _ : _ : _ : ps) = replicate 6 n ++ extractQuadNormals ps where
    t = p3 - p2 -- tangent
    b = p1 - p2 -- bitangent
    n = LP.normalize (LP.cross b t) -- normal (right-handed cross product)

extractQuadTangents :: [V3 GLfloat] -> [V3 GLfloat]
extractQuadTangents [] = []
extractQuadTangents (p1 : p2 : p3 : _ : _ : _ : ps) = replicate 6 t ++ extractQuadTangents ps where
    t = LP.normalize (p3 - p2)

createTexturedSkyBox :: GLfloat -> IO (VertexArrayObject, Render, Dispose)
createTexturedSkyBox edgeSize = do
    let a = edgeSize / 2
        z = -a
        points = createBaseBox edgeSize
        texCoords = concat $ for [0..5] $ \z ->
            [ 0, 1, z
            , 0, 0, z
            , 1, 0, z
            , 0, 1, z
            , 1, 0, z
            , 1, 1, z
            ]
        triangles = concatMap (\i -> let i3 = i*3 in [points !! i3, points !! (i3+1), points !! (i3+2)])
            [ 2, 0, 1
            , 2, 1, 3
            , 7, 5, 4
            , 7, 4, 6
            , 0, 4, 5
            , 0, 5, 1
            , 1, 5, 7
            , 1, 7, 3
            , 3, 7, 6
            , 3, 6, 2
            , 2, 6, 4
            , 2, 4, 0
            ]
    createObject Triangles (Just triangles) Nothing (Just texCoords) Nothing Nothing Nothing

createTexturedMesh :: IO (VertexArrayObject, Render, Dispose)
createTexturedMesh = do
    let
        heights =
            [ 4, 2, 3, 1
            , 3, 5, 8, 2
            , 7, 1, 12, 6
            , 4, 6, 8, 3
            ] :: [GLfloat]

        edgeSize = 4
        (xSize, zSize) = (40, 40)
        toWorld iSize i = i * iSize / edgeSize - iSize / 2
        points = concat $ zipWith (\(z, x) y -> [toWorld xSize x, y, toWorld zSize z, x / 2, z / 2])
            [(z, x) | z <- [0..edgeSize-1], x <- [0..edgeSize-1]]
            heights

        ri = fromIntegral (length heights)
        indices =
            [ 0, 4, 1, 5, 2, 6, 3, 7, ri    -- First row, then restart
            , 4, 8, 5, 9, 6, 10, 7, 11, ri  -- Second row, then restart
            , 8, 12, 9, 13, 10, 14, 11, 15  -- Third row, no restart
            ] :: [GLuint]

    createIndexedMultiplexedObject (points, [(pointLocation, 3), (texCoordLocation, 2)]) indices $
        \ix t p -> do
            primitiveRestartIndex $= Just (fromIntegral ri)
            drawElements TriangleStrip ix t p

createMesh :: Heightmap Float -> V3 Float -> IO (VertexArrayObject, Render, Dispose)
createMesh heightmap@(width, height, altitudes) (V3 sx sy sz) =
    createMultiplexedObject (points, [(pointLocation, 3)]) (drawArrays Points)
    where
        toPoint :: Int -> Float -> [Float]
        toPoint i z =
            let (x, y) = indexToXY heightmap i
                [x', y', w', h'] = map fromIntegral [x, y, width, height]
            in  [ (x' - w' / 2) * sx
                , (y' - h' / 2) * sy
                , z * sz
                ]
        points = concat $ zipWith toPoint [0..] (V.toList altitudes)

createRandomMesh :: Heightmap Float -> V3 Float -> IO (VertexArrayObject, Render, Dispose)
createRandomMesh heightmap@(width, height, altitudes) (V3 sx sy sz) = do
    points <- concat <$> zipWithM toPoint [0..] (V.toList altitudes)
    createMultiplexedObject (points, [(pointLocation, 3)]) (drawArrays Points)
    where
        toPoint :: Int -> Float -> IO [Float]
        toPoint i z = do
            (dx, dy) <- runRandomIO $ (,) <$> getRandomR (-0.25, 0.25) <*> getRandomR (-0.25, 0.25)
            let (x, y) = indexToXY heightmap i
                [x', y', w', h'] = map fromIntegral [x, y, width, height]
                (x'', y'') = (x' + dx, y' + dy)
                z' = getInterpolatedHeightmapValue heightmap (x'', y'')
            return
                [ (x'' - w' / 2) * sx
                , (y'' - h' / 2) * sy
                , z' * sz
                ]

-- Restart index pas supporté en 1.3
createTerrain130 :: Heightmap Float -> V3 Float -> IO (VertexArrayObject, Render, Dispose)
createTerrain130 heightmap scale =
    createIndexedMultiplexedObject (points, [(pointLocation, 3), (texCoordLocation, 2), (normalLocation, 3)]) indices $
        \ix t p -> drawElements Triangles ix t p
    where
        (width, height, altitudes) = heightmap
        points = preCreateTerrain heightmap scale
        indices = map fromIntegral $
            concatMap (\y ->
                concatMap (\x -> map (xyToIndex heightmap)
                    [ (x, y)
                    , (x, y + 1)
                    , (1 + x, y)
                    , (1 + x, y)
                    , (x, y + 1)
                    , (x + 1, y + 1)])
                [0..width-2])
                [0..height-2]

createTerrain :: Heightmap Float -> V3 Float -> IO (VertexArrayObject, Render, Dispose)
createTerrain heightmap scale =
    createIndexedMultiplexedObject (points, [(pointLocation, 3), (texCoordLocation, 2), (normalLocation, 3)]) indices $
        \ix t p -> do
            primitiveRestartIndex $= Just (fromIntegral restartIndex)
            drawElements TriangleStrip ix t p
    where
        (width, height, altitudes) = heightmap
        points = preCreateTerrain heightmap scale
        restartIndex = width * height
        indices = map fromIntegral $
            concatMap (\y ->
                concatMap (\x ->
                    [x + y * width, x + (y + 1) * width]) [0..width-1] ++ [restartIndex])
                [0..height-2]

preCreateTerrain :: Heightmap Float -> V3 Float -> [Float]
preCreateTerrain heightmap@(width, height, altitudes) (V3 sx sy sz) = points
    where
        getAltitude = getHeightmapValue heightmap
        calculateNormal (x, y) = LP.normalize n where
            z = getAltitude (x, y)
            getNeighbour dx dy = V3 (fromIntegral dx) (fromIntegral dy) (getAltitude (x + dx, y + dy) - z)
            eastZ = getNeighbour 1 0
            westZ = getNeighbour (-1) 0
            northZ = getNeighbour 0 1
            southZ = getNeighbour 0 (-1)
            eastNorth = eastZ `LP.cross` northZ
            northWest = northZ `LP.cross` westZ
            westSouth = westZ `LP.cross` southZ
            southEast = southZ `LP.cross` eastZ
            n = westSouth + southEast + eastNorth + northWest

        toPoint :: Int -> Float -> [Float]
        toPoint i z =
            let (x, y) = indexToXY heightmap i
                [x', y', w', h'] = map fromIntegral [x, y, width, height]
                (V3 nx ny nz) = calculateNormal (x, y)
                (V3 nx' ny' nz') = LP.normalize (V3 (nx / sx) (ny / sy) (nz / sz))
            in  [ (x' - w' / 2) * sx
                , (y' - h' / 2) * sy
                , z * sz
                , x' * sx
                , y' * sy
                , nx', ny', nz'
                ]
        points = concat $ zipWith toPoint [0..] (V.toList altitudes)

createFlatTerrain :: (Int, Int) -> IO (VertexArrayObject, Render, Dispose)
createFlatTerrain (width, height) =
    createIndexedMultiplexedObject (points, [(pointLocation, 3), (texCoordLocation, 2)]) indices $
        \ix t p -> do
            primitiveRestartIndex $= Just (fromIntegral restartIndex)
            drawElements TriangleStrip ix t p
    where
        toPoint i z =
            let (y, x) = divMod i width
                z' = fromIntegral z
                y' = fromIntegral y
                x' = fromIntegral x
            in  [ x' - fromIntegral width / 2
                , y' - fromIntegral height / 2
                , 0
                , x'
                , y']
        points = concat $ zipWith toPoint [0..] (replicate (width * height) 0)

        restartIndex = width * height
        indices = map fromIntegral $
            concatMap
                (\y -> concatMap
                    (\x -> [x + y * width, x + (y + 1) * width])
                    [0..width-1]
                    ++
                    [restartIndex])
                [0..height-2]

----------------------------------------------------------------------------------------------------

createShadowFrameBuffer :: Size -> IO (FramebufferObject, TextureObject, Dispose)
createShadowFrameBuffer size = do
    (fbo, [texture], dispose) <- createFrameBuffer size [] (Just True)
    return (fbo, texture, dispose)

createGeometryFrameBuffer :: Bool -> Size -> IO (FramebufferObject, [TextureObject], Dispose)
createGeometryFrameBuffer useDepthTexture size =
    createFrameBuffer size colorSpecs (Just useDepthTexture)
    where colorSpecs =
            [ (RGB16F, RGB)
            , (RGB16F, RGB)
            , (RGBA16F, RGBA)
            ]

createSimpleFrameBuffer :: Size -> IO (FramebufferObject, TextureObject, Dispose)
createSimpleFrameBuffer size = do
    (fbo, [texture], dispose) <- createFrameBuffer size [(R16F, Red)] Nothing
    return (fbo, texture, dispose)

createHdrFrameBuffer :: Size -> IO (FramebufferObject, TextureObject, Dispose)
createHdrFrameBuffer size = do
    (fbo, [texture], dispose) <- createFrameBuffer size [(RGBA16F, RGBA)] (Just False) -- Alpha channel matters!
    return (fbo, texture, dispose)

createBloomFrameBuffer :: Size -> IO (FramebufferObject, [TextureObject], Dispose)
createBloomFrameBuffer size = createFrameBuffer size [(RGB8, RGB), (RGB8, RGB)] Nothing

createColorFrameBuffer :: Size -> IO (FramebufferObject, TextureObject, Dispose)
createColorFrameBuffer size = do
    (fbo, [texture], dispose) <- createFrameBuffer size [(RGB8, RGB)] Nothing
    return (fbo, texture, dispose)

createFrameBuffer
    :: Size
    -> [(PixelInternalFormat, PixelFormat)]
    -> Maybe Bool
    -> IO (FramebufferObject, [TextureObject], Dispose)
createFrameBuffer (Size w h) colorBufferSpecs depthBufferSpec = do
    fbo <- genObjectName :: IO FramebufferObject
    bindFramebuffer DrawFramebuffer $= fbo

    let colorBufferIndices = take (length colorBufferSpecs) [0..] :: [GLsizei]

    -- color buffers
    textures <- forM (zip colorBufferIndices colorBufferSpecs) $
        \(i, (pixelInternalFormat, pixelFormat)) -> do
            texture <- genObjectName :: IO TextureObject
            textureBinding Texture2D $= Just texture
            texImage2D
                Texture2D
                NoProxy
                0 -- Mipmap level
                pixelInternalFormat
                (TextureSize2D w h)
                0 -- No borders
                (PixelData pixelFormat Float nullPtr)
            textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
            framebufferTexture2D DrawFramebuffer (ColorAttachment (fromIntegral i)) Texture2D texture 0
            return texture

    -- tell OpenGL which color attachments we'll use (of this framebuffer) for rendering
    drawBuffers $= map FBOColorAttachment colorBufferIndices

    (allTextures, dispose) <- case depthBufferSpec of

        {- in https://learnopengl.com/#!Advanced-Lighting/SSAO:
        It is possible to reconstruct the actual position vectors from depth values alone using some
        clever tricks as Matt Pettineo described in his blog. This requires some extra calculations
        in the shaders, but saves us from having to store position data in the G-buffer which costs
        a lot of memory. For the sake of a simple example, we'll leave these optimizations out of
        the tutorial.

        Conlusion: it implies a tradeoff between speed and space.
        -}
        Just True -> do
            -- A depth texture is slower than a depth buffer, but you can sample it later.
            depthTexture <- genObjectName :: IO TextureObject
            textureBinding Texture2D $= Just depthTexture
            texImage2D
                Texture2D
                NoProxy
                0 -- Mipmap level
                DepthComponent'
                (TextureSize2D w h)
                0 -- No borders
                (PixelData DepthComponent Float nullPtr)
            textureWrapMode Texture2D S $= (Repeated, ClampToEdge) -- (Repeated, Repeat)
            textureWrapMode Texture2D T $= (Repeated, ClampToEdge) -- (Repeated, Repeat)
            textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
            framebufferTexture2D DrawFramebuffer DepthAttachment Texture2D depthTexture 0

            let dispose = do
                    deleteObjectName depthTexture
                    mapM_ deleteObjectName textures
                    deleteObjectName fbo
            return (depthTexture : textures, dispose)

        Just False -> do
            -- Add render buffer object as depth buffer...
            depthRenderBuffer <- genObjectName :: IO RenderbufferObject
            bindRenderbuffer Renderbuffer $= depthRenderBuffer
            renderbufferStorage Renderbuffer DepthComponent' (RenderbufferSize w h)
            framebufferRenderbuffer Framebuffer DepthAttachment Renderbuffer depthRenderBuffer

            let dispose = do
                    deleteObjectName depthRenderBuffer
                    mapM_ deleteObjectName textures
                    deleteObjectName fbo
            return (textures, dispose)

        Nothing -> do
            let dispose = do
                    mapM_ deleteObjectName textures
                    deleteObjectName fbo
            return (textures, dispose)

    -- ...and check for completeness.
    Complete <- framebufferStatus Framebuffer

    bindFramebuffer DrawFramebuffer $= defaultFramebufferObject

    return (fbo, allTextures, dispose)

----------------------------------------------------------------------------------------------------

createMultiplexedObject
    :: ([GLfloat], [(GLuint, GLint)])
    -> (GLint -> GLint -> IO ())
    -> IO (VertexArrayObject, Render, Dispose)
createMultiplexedObject (attrib, attribContent) draw = do
    vao <- genObjectName
    vbo <- withBinding bindVertexArrayObject vao (createMultiplexedVbo attrib attribContent)
    let jump = sum (map snd attribContent)
        size = fromIntegral (length attrib) `div` jump
        render = withBinding bindVertexArrayObject vao $ draw 0 (jump * size)

    return (vao, const render, deleteObjectName vbo >> deleteObjectName vao)

createIndexedMultiplexedObject
    :: ([GLfloat], [(GLuint, GLint)])
    -> [GLuint]
    -> (NumArrayIndices -> DataType -> Ptr a -> IO ())
    -> IO (VertexArrayObject, Render, Dispose)
createIndexedMultiplexedObject (attrib, attribContent) indices draw = do
    vao <- genObjectName
    (render, dispose) <- withBinding bindVertexArrayObject vao $ do
        createMultiplexedVbo attrib attribContent

        let jump = sum (map snd attribContent)
            size = fromIntegral (length attrib) `div` jump

        -- L’IBO ne peut être créé sans VAO lié (pourquoi ?)
        indexIbo <- createIbo indices
        let render = withBinding bindVertexArrayObject vao $ do
                -- L’IBO *est* lié au VAO (c’est logique... ou pas).
                bindBuffer ElementArrayBuffer $= Just indexIbo
                draw (fromIntegral $ length indices) UnsignedInt nullPtr
        return (render, deleteObjectName indexIbo)

    return (vao, const render, dispose >> deleteObjectName vao)

createMultiplexedVbo :: [GLfloat] -> [(GLuint, GLint)] -> IO BufferObject
createMultiplexedVbo vertices attribContent = do
    vbo <- genObjectName
    withBuffer ArrayBuffer vbo $ do
        let vertexVector =  V.fromList vertices :: V.Vector GLfloat
        V.unsafeWith vertexVector $ \p ->
            bufferData ArrayBuffer $= (getBytePtrSize vertexVector, p, StaticDraw)

        let jump = sum (map snd attribContent)
            offsets = scanl (+) 0 (map snd attribContent)
        forM_ (zip attribContent offsets) $ \((index, size), offset) -> do
            let attribute = AttribLocation index
                floatSize = sizeOf (0 :: GLfloat)
                stride = jump * fromIntegral floatSize
                ptr = nullPtr `plusPtr` (fromIntegral offset * floatSize)
            -- C’est ici que le VAO mémorise son association avec les donnés du VBO "bindé".
            -- (Le binding en lui-même n’intéresse pas le VAO et n’est pas mémorisé par lui.)
            vertexAttribPointer attribute $= (ToFloat, VertexArrayDescriptor size Float stride ptr)
            vertexAttribArray attribute $= Enabled

    return vbo

createObject
    :: PrimitiveMode
    -> Maybe [GLfloat]
    -> Maybe [GLfloat]
    -> Maybe [GLfloat]
    -> Maybe [GLfloat]
    -> Maybe [GLfloat]
    -> Maybe [GLuint]
    -> IO (VertexArrayObject, Render, Dispose)
createObject renderMode mPoints mColors mTexCoords mNormals mTangents mIndices = do
    vao <- genObjectName
    (render, dispose) <- withBinding bindVertexArrayObject vao $ do

        let mLength = fromIntegral . length . fromJust

            createVbo :: NumComponents -> [GLfloat] -> GLuint -> IO BufferObject
            createVbo elementSize vertices locationIndex = do
                let pointAttribute = AttribLocation locationIndex
                    vertexVector =  V.fromList vertices :: V.Vector GLfloat
                vbo <- genObjectName
                -- Ici, un VAO doit aussi avoir été bindé ? Non...
                withBuffer ArrayBuffer vbo $ do
                    V.unsafeWith vertexVector $ \p ->
                        bufferData ArrayBuffer $= (getBytePtrSize vertexVector, p, StaticDraw)
                    -- Mais ici, oui.
                    vertexAttribPointer pointAttribute $= (ToFloat, VertexArrayDescriptor elementSize Float 0 nullPtr)
                    vertexAttribArray pointAttribute $= Enabled
                return vbo

        case mPoints of
            Just points -> Just <$> createVbo 3 points pointLocation
            Nothing -> return Nothing

        case mColors of
            Just colors -> Just <$> createVbo 3 colors colorLocation
            Nothing -> return Nothing

        let texCoordElementSize = mLength mTexCoords `div` (mLength mPoints `div` 3)
        case mTexCoords of
            Just texCoords -> Just <$> createVbo texCoordElementSize texCoords texCoordLocation
            Nothing -> return Nothing

        case mNormals of
            Just normals -> Just <$> createVbo 3 normals normalLocation
            Nothing -> return Nothing

        case mTangents of
            Just tangents -> Just <$> createVbo 3 tangents tangentLocation
            Nothing -> return Nothing

        case mIndices of
            Nothing -> return (withBinding bindVertexArrayObject vao $ drawArrays renderMode 0 (mLength mPoints), doNothing)
            Just indices -> do
                indexIbo <- createIbo indices
                let render = do
                        -- L’IBO n’est pas lié au VAO (c’est logique).
                        bindBuffer ElementArrayBuffer $= Just indexIbo
                        withBinding bindVertexArrayObject vao $
                            drawElements renderMode (fromIntegral $ length indices) UnsignedInt nullPtr
                return (render, deleteObjectName indexIbo)

    return (vao, const render, dispose >> deleteObjectName vao)

createIbo :: [GLuint] -> IO BufferObject
createIbo indices = do
    let indexVector =  V.fromList indices :: V.Vector GLuint

    ibo <- genObjectName
    -- Ici, un VAO doit aussi avoir été bindé ? Non.
    withBuffer ElementArrayBuffer ibo $
        V.unsafeWith indexVector $ \p ->
            bufferData ElementArrayBuffer $= (getBytePtrSize indexVector, p, StaticDraw)

    return ibo

----------------------------------------------------------------------------------------------------

class HasSize a where
    getByteCount :: a -> NumArrayIndices
    getByteCount v = fromIntegral (getByteSize v)
    getBytePtrSize :: a -> GLsizeiptr
    getBytePtrSize v = fromIntegral (getByteSize v)
    getByteSize :: a -> Int

instance HasSize (V.Vector GLuint) where
    getByteSize v = V.length v * sizeOf (0 :: GLuint)

instance HasSize (V.Vector GLfloat) where
    getByteSize v = V.length v * sizeOf (0 :: GLfloat)

flattenMatrix :: (Epsilon a, Floating a) => M44 a -> [a]
flattenMatrix (V4 (V4 m11 m12 m13 m14) (V4 m21 m22 m23 m24) (V4 m31 m32 m33 m34) (V4 m41 m42 m43 m44)) =
    [ m11, m12, m13, m14
    , m21, m22, m23, m24
    , m31, m32, m33, m34
    , m41, m42, m43, m44
    ]

flattenVertices :: [V3 a] -> [a]
flattenVertices = concatMap (\(V3 x y z) -> [x, y, z])

unflattenVertices3 :: [a] -> [V3 a]
unflattenVertices3 [] = []
unflattenVertices3 (x : y : z : cs) = V3 x y z : unflattenVertices3 cs
