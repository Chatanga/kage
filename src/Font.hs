module Font (
    createText,
    createDistanceFieldAtlas,
    createDistanceFieldLetter
) where

import System.Log.Logger
import Control.Monad
import Control.Monad.State
import Control.Arrow
import Control.Parallel.Strategies
import Data.Bits
import Data.Char
import Data.List
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.Ord
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Foreign (allocaArray, alloca, peek, plusPtr, withArray, peekArray, nullPtr, Ptr)
import Foreign.C.String (withCString, CString)
import Foreign.C.Types (CChar)

import Codec.Picture
import Codec.Picture.Types

import Graphics.Rendering.OpenGL hiding (bitmap)

import Graphics.Rendering.FreeType.Internal
import Graphics.Rendering.FreeType.Internal.Bitmap (width, rows, pitch, num_grays, pixel_mode, palette_mode, buffer, FT_Bitmap)
import Graphics.Rendering.FreeType.Internal.BitmapSize (FT_Bitmap_Size)
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes -- (FT_Error, FT_Glyph_Format)
import Graphics.Rendering.FreeType.Internal.Library (FT_Library)
import Graphics.Rendering.FreeType.Internal.Face (FT_Face, glyph, num_glyphs, num_fixed_sizes, available_sizes)
import Graphics.Rendering.FreeType.Internal.FaceType ()
import Graphics.Rendering.FreeType.Internal.GlyphSlot (FT_GlyphSlot, format, linearHoriAdvance, linearVertAdvance, advance, bitmap_left, bitmap_top, bitmap)

import Linear as LP hiding (distance)

import Debug
import Error
import FunctionalGL as FGL
import Shader
import Texture
import Buffer

----------------------------------------------------------------------------------------------------

createText :: LP.V3 Float -> LP.V3 Float -> LP.V3 Float -> String -> ResourceIO Renderable
createText position up advance text = do
    atlas <- liftIO getAtlas

    let charInfos = mapMaybe ((`IntMap.lookup` atlas) . fromEnum) text
        (triangles, texCoords) = (concat *** concat) . unzip . snd $ mapAccumL toLetter position charInfos

        toLetter :: LP.V3 Float -> CharInfo -> (LP.V3 Float, ([Float], [Float]))
        toLetter p (CharInfo cc b e l t w r) = (p', (triangles, texCoords)) where
            [b', e', l', t', w', r'] = map fromIntegral [b, e, l, t, w, r]
            p' = p ^+^ ((w' - l') *^ advance)
            p1 = p ^-^ (l' *^ advance) ^+^ (up ^* t')
            p2 = p1 ^+^ (w' *^ advance)
            p3 = p2 ^-^ (up ^* r')
            p4 = p1 ^-^ (up ^* r')
            triangles = flattenVertices [p1, p2, p3, p1, p3, p4]
            texCoords = map ((/ 2048) . fromIntegral) [b, e, b+w, e, b+w, e+r, b, e, b+w, e+r, b, e+r]

    (vao, render, dispose) <- liftIO $
        createObject Triangles (Just triangles) Nothing (Just texCoords) Nothing Nothing Nothing

    Just (texture, disposeTexture) <- withAcquiredImage "data/atlas.png" $ \t -> do
        -- generateMipmap' Texture2D
        textureWrapMode Texture2D S $= (Repeated, Repeat)
        textureWrapMode Texture2D T $= (Repeated, Repeat)
        textureFilter Texture2D $= ((Linear', Nothing), Linear')

    (program, disposeProgram) <- acquireProgramWithShaders' "letter_vs.glsl" "letter_fs.glsl"

    let render' p =
            usingOrderedTextures p [texture] .
                FGL.withState blend Enabled .
                FGL.withState blendFunc (SrcAlpha, OneMinusSrcAlpha) .
                FGL.withState depthMask Disabled .
                FGL.withState cullFace Nothing $ render p

    return $ Renderable
        [(DirectShadingStage, program)]
        vao
        render'
        (disposeTexture >> disposeProgram >> liftIO dispose)

----------------------------------------------------------------------------------------------------

assertEquals :: (Show a, Eq a) => a -> a -> IO ()
assertEquals x y = unless (x == y) $ fail $ "Expected " ++ show x ++ " but got " ++ show y

runFreeType :: IO FT_Error -> IO ()
runFreeType m = do
    r <- m
    unless (r == 0) $ fail $ "FreeType Error: " ++ show r

ftInitFreeType :: IO FT_Library
ftInitFreeType = alloca $ \ptr -> do
    runFreeType $ ft_Init_FreeType ptr
    peek ptr

ftLibraryVersion :: FT_Library -> IO (FT_Int, FT_Int, FT_Int)
ftLibraryVersion ft = alloca $ \xPtr -> alloca $ \yPtr -> alloca $ \zPtr -> do
    ft_Library_Version ft xPtr yPtr zPtr
    [x, y, z] <- mapM peek [xPtr, yPtr, zPtr]
    return (x, y, z)

ftNewFace :: FT_Library -> FilePath -> IO FT_Face
ftNewFace ft fp = withCString fp $ \str ->
    alloca $ \ptr -> do
        runFreeType $ ft_New_Face ft str 0 ptr
        peek ptr

type Point = (Int, Int)

getAtlas :: IO (IntMap.IntMap CharInfo)
getAtlas = do
    entries <- map (map read . words) . lines <$> readFile "data/atlas.txt"
    return $ IntMap.fromList $ flip map entries $ \[cc, b, e, l, t, w, r] -> (cc, CharInfo cc b e l t w r)

-- FreeType (http://freetype.org/freetype2/docs/tutorial/step1.html)
createDistanceFieldAtlas :: FilePath -> IO ()
createDistanceFieldAtlas path = do
    ft <- ftInitFreeType
    (x, y, z) <- ftLibraryVersion ft
    infoM "Kage" ("FT library version: " ++ show x ++ "." ++ show y ++ "." ++ show z)

    face <- ftNewFace ft path
    runFreeType $ ft_Set_Pixel_Sizes face 512 0

    let atlasWidth = 2048
        atlasHeight = 2048
        scale = 8

        ascii = [0x20..0xff]
        hiragana = [0x3040..0x309f]
        katakana = [0x30a0..0x30ff]

    charData <- forM (ascii ++ hiragana ++ katakana) (createDistanceFieldFromCharCode face)

    let (atlasCharData, atlasChanges) = unzip . snd $ mapAccumL insertCharImage ((0, 0), 0) charData

        scaleSize (w, h) = ((w + scale - 1) `div` scale, (h + scale - 1) `div` scale)

        insertCharImage :: (Point, Int) -> (CharInfo, ImageSource) -> ((Point, Int), (CharInfo, [(Int, Int)]))
        insertCharImage (offset@(xOffset, yOffset), hMax) (charInfo, source)
            | xOffset + w' <= atlasWidth = (((xOffset + w', yOffset), max hMax h'), newCharInfo offset)
            | yOffset + h' <= atlasHeight = (((w', yOffset + hMax), h'), newCharInfo (0, yOffset + hMax))
            | otherwise = error "No more place on the atlas texture"
            where
                (w', h') = scaleSize (isWitdh source, isRow source)
                (l, t) = (ciLeft charInfo `div` scale, ciTop charInfo `div` scale)
                newCharInfo (dx, dy) = (CharInfo (ciCharCode charInfo) dx dy l t w' h', copyImage (dx, dy) source)

        copyImage :: (Int, Int) -> ImageSource -> [(Int, Int)]
        copyImage offset imageSource@(ImageSource w h readCharPixel) = changes where
            (w', h') = scaleSize (w, h)
            pixels = [(x, y) | x <- [0 .. w' - 1], y <- [0 .. h' - 1]]
            changes = map (\(x, y) -> (toIndex offset x y, average imageSource x y)) pixels

        toIndex (xOffset, yOffset) x y = xOffset + x + (yOffset + y) * atlasWidth

        average (ImageSource w h readCharPixel) x y = 255 - (sum values `div` length values) where
            values = map (uncurry readCharPixel)
                [   (x', y')
                |   x' <- [x * scale .. min (w - 1) (x * scale + scale - 1)]
                ,   y' <- [y * scale .. min (h - 1) (y * scale + scale - 1)]
                ]

        atlas = UV.replicate (atlasWidth * atlasHeight) 0 UV.// concat (atlasChanges `using` parList rdeepseq)

        readAtlasPixel x y = fromIntegral $ atlas UV.! (x + y * atlasWidth)

    writeFile "atlas.txt" $ intercalate "\n" $ flip map atlasCharData $
        \(CharInfo cc b e l t w r) -> unwords (map show [cc, b, e, l, t, w, r])

    savePngImage "atlas.png" (ImageY8 (generateImage readAtlasPixel atlasWidth atlasHeight))

    -- Discard the face object, as well as all of its child slots and sizes.
    runFreeType $ ft_Done_FreeType ft

createDistanceFieldLetter :: FilePath -> Char -> IO ()
createDistanceFieldLetter path char = do
    ft <- ftInitFreeType
    (x, y, z) <- ftLibraryVersion ft
    infoM "Kage" ("FT library version: " ++ show x ++ "." ++ show y ++ "." ++ show z)

    face <- ftNewFace ft path
    runFreeType $ ft_Set_Pixel_Sizes face 512 0

    (ImageSource w r readPixel) <- snd <$> createDistanceFieldFromCharCode face (fromEnum char)

    let readPixel' x y = fromIntegral . min 255 . (* 8) . round . sqrt . fromIntegral $ readPixel x y
        finalImage = generateImage readPixel' w r :: Image Pixel8

    savePngImage ("letter-" ++ char : ".png") (ImageY8 finalImage)

    -- Discard the face object, as well as all of its child slots and sizes.
    runFreeType $ ft_Done_FreeType ft

data CharInfo = CharInfo
    {   ciCharCode :: Int
    ,   ciBegin :: Int
    ,   ciEnd :: Int
    ,   ciLeft :: Int
    ,   ciTop :: Int
    ,   ciWidth :: Int
    ,   ciRow :: Int
    }

createDistanceFieldFromCharacter :: FT_Face -> Char -> IO (CharInfo, ImageSource)
createDistanceFieldFromCharacter face character = createDistanceFieldFromCharCode face (fromEnum character)

createDistanceFieldFromCharCode :: FT_Face -> Int -> IO (CharInfo, ImageSource)
createDistanceFieldFromCharCode face charCode = do

    -- retrieve glyph index from (Unicode) character code
    glyphIndex <- ft_Get_Char_Index face $ fromIntegral charCode

    -- load glyph image into the slot (erase previous one)
    runFreeType $ ft_Load_Char face (fromIntegral charCode) ft_LOAD_DEFAULT

    slot <- peek $ glyph face :: IO FT_GlyphSlot

    -- convert to 2-bit bitmap
    runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_MONO

    assertEquals ft_GLYPH_FORMAT_BITMAP =<< peek (format slot)

    left <- fromIntegral <$> peek (bitmap_left slot)
    top <- fromIntegral <$> peek (bitmap_top slot)

    infoM "Kage" $ "Font texture position: " ++ show left ++ " x " ++ show top

    -- Get the char bitmap.
    bitmap <- peek $ bitmap slot :: IO FT_Bitmap

    let w = fromIntegral $ width bitmap
        r = fromIntegral $ rows bitmap
        p = fromIntegral $ pitch bitmap

    assertEquals (((w + 15) `div` 16) * 2) p
    assertEquals 0 (num_grays bitmap)
    assertEquals 1 (pixel_mode bitmap)
    assertEquals 0 (palette_mode bitmap)

    infoM "Kage" $ "Font texture size: " ++ show w ++ " x " ++ show r

    -- Get the raw bitmap data.
    bitmapData <- V.fromList <$> peekArray (p * r) (buffer bitmap :: CString) :: IO (V.Vector CChar) -- CString == Ptr CChar

    return (CharInfo charCode 0 0 left top w r, createSignedDistanceField bitmapData (w, r, p))

data ImageSource = ImageSource
    {   isWitdh :: Int
    ,   isRow :: Int
    ,   isReadPixel :: Int -> Int -> Int
    }

createDistanceField :: V.Vector CChar -> (Int, Int, Int) -> ImageSource
createDistanceField bitmapData (w, r, p) = ImageSource (xEnd + 1) (yEnd + 1) readPixel where
    scale = 4
    border = 512 `div` scale
    xEnd = w + border * 2 - 1
    yEnd = r + border * 2 - 1

    -- Not signed actually.
    sdf :: UV.Vector Double
    sdf = UV.fromList $ snd $ mapAccumL findNearest Nothing circuit where
        -- We scan the lines like a snake in order to only move one cell at a time.
        circuit = [(if even y then x else xEnd - x, y) | y <- [0 .. yEnd],  x <- [0 .. xEnd]]

    findNearest :: Maybe (Point, Point) -> Point -> (Maybe (Point, Point), Double)

    findNearest Nothing p' = (Just (n', p'), getSqrt d2) where
        area = [(x, y)  | y <- [border .. yEnd - border]
                        , x <- [border .. xEnd - border]
                        ]
        (n', d2) = findMin area p'

    findNearest (Just (n@(nx, ny), p@(px, py))) p'@(px', py') = (Just (n', p'), d'') where
        (n', d'') = if py == py' && py == ny && ((px < nx) == (px < px'))
            then (n, fromIntegral $ abs (px' - nx))
            else
                let d = floor $ distance n p
                    d' = ceiling $ distance n p'
                    -- Don't search a better point farther than the previous nearest node (roughly).
                    (yMin, yMax) = (py' - d', py' + d')
                    (xMin, xMax) = (px' - d', px' + d')
                    -- The new nearest point is necessarily in the same direction as our scanning.
                    ((yMin', yMax'), (xMin', xMax'))
                        | px < px' = ((yMin, yMax), (nx, xMax))
                        | px > px' = ((yMin, yMax), (xMin, nx))
                        | otherwise = ((ny, yMax), (xMin, xMax))
                    area = [(x, y)  | y <- [(max border yMin') .. (min (yEnd - border) yMax')]
                                    , x <- [(max border xMin') .. (min (xEnd - border) xMax')]
                                    -- Don't search a better point farther than the previous nearest node.
                                    , circle p' d' (x, y) < 1
                                    -- Don't search inside the previous search area.
                                    , circle p d (x, y) >= 0
                                    ]
                    (n', d2) = findMin area p'
                in  (n', getSqrt d2)

    findMin :: [Point] -> Point -> (Point, Int)
    findMin area p = minimumBy (comparing snd) (if null area' then [(p, 0)] else area')
        where area' = mapMaybe f area
              f n = if isIn n then Just (n, squareDistance n p) else Nothing

    isIn (x, y) = testBit (bitmapData V.! ((x' `shiftR` 3) + y' * p)) (7 - (x' .&. 7))
        where (x', y') = (x - border, y - border)

    readPixel x y = min 255 . (* scale) . round $ d
        where d = sdf UV.! ((if even y then x else xEnd - x) + y * (xEnd + 1))

createSignedDistanceField :: V.Vector CChar -> (Int, Int, Int) -> ImageSource
createSignedDistanceField bitmapData (w, r, p) = ImageSource (xEnd + 1) (yEnd + 1) readPixel where
    scale = 4
    border = 512 `div` scale
    xEnd = w + border * 2 - 1
    yEnd = r + border * 2 - 1

    -- Not signed actually.
    sdf :: UV.Vector Double
    sdf = UV.fromList $ snd $ mapAccumL findNearest Nothing circuit where
        -- We scan the lines like a snake in order to only move one cell at a time.
        circuit = [(if even y then x else xEnd - x, y) | y <- [0 .. yEnd],  x <- [0 .. xEnd]]

    findNearest :: Maybe (Point, Point) -> Point -> (Maybe (Point, Point), Double)

    findNearest Nothing p' = (Just (n', p'), getSqrt d2) where
        area = [(x, y)  | y <- [border - 1 .. yEnd - border + 1]
                        , x <- [border - 1 .. xEnd - border + 1]
                        ]
        (n', d2) = findMin area p'

    findNearest (Just (n@(nx, ny), p@(px, py))) p'@(px', py') = (Just (n', p'), d''') where
        outside = is False p
        outside' = is False p'
        d''' = if outside' then d'' else 1 - d''
        (n', d'')
            -- | n == p' = (p, 1)
            | outside /= outside' = (p, 1)
            | py == py' && py == ny && ((px < nx) == (px < px')) = (n, fromIntegral $ abs (px' - nx))
            | otherwise =
                let d = floor $ distance n p
                    d' = ceiling $ distance n p'
                    -- Don't search a better point farther than the previous nearest node (roughly).
                    (yMin, yMax) = (py' - d', py' + d')
                    (xMin, xMax) = (px' - d', px' + d')
                    -- The new nearest point is necessarily in the same direction as our scanning.
                    ((yMin', yMax'), (xMin', xMax'))
                        | px < px' = ((yMin, yMax), (nx, xMax))
                        | px > px' = ((yMin, yMax), (xMin, nx))
                        | otherwise = ((ny, yMax), (xMin, xMax))
                    area = [(x, y)  | y <- [max (border - 1) yMin' .. min (yEnd - border + 1) yMax']
                                    , x <- [max (border - 1) xMin' .. min (xEnd - border + 1) xMax']
                                    -- Don't search a better point farther than the previous nearest node.
                                    , circle p' d' (x, y) < 1
                                    -- Don't look inside the previous search area.
                                    , circle p d (x, y) >= 0
                                    ]
                    (n', d2) = findMin area p'
                in  (n', getSqrt d2)

    findMin :: [Point] -> Point -> (Point, Int)
    findMin area p@(x, y) = minimumBy (comparing (abs . snd)) (if null area' then [(p, 0)] else area')
        where inside = is False p
              area' = mapMaybe f area
              f a = if is inside a then Just (a, squareDistance a p) else Nothing

    is inside (x, y) = if outside then not inside else result
        where outside = x < border || x >= w + border || y < border || y >= r + border
              result = inside == testBit (bitmapData V.! ((x' `shiftR` 3) + y' * p)) (7 - (x' .&. 7))
              (x', y') = (x - border, y - border)

    readPixel x y = min 255 . (+ 127) . round $ d
        where d = sdf UV.! ((if even y then x else xEnd - x) + y * (xEnd + 1))

----------------------------------------------------------------------------------------------------

squareDistance :: Point -> Point -> Int
squareDistance (ax, ay) (bx, by) = (ax - bx)^2 + (ay - by)^2

distance :: Point -> Point -> Double
distance a b = getSqrt (squareDistance a b)

circle (cx, cy) r (x, y) = (x - cx)^2 + (y - cy)^2 - r^2

-- Very slight benefit...
cachedRoots = UV.generate 2048 (sqrt . fromIntegral) :: UV.Vector Double
getSqrt x = if x < 2048 then cachedRoots UV.! x else (sqrt . fromIntegral) x

