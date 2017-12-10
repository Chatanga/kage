module Texture
    ( acquireImage
    , loadImage
    , loadImageR8
    , loadHeightmap
    , withLoadedImage
    , saveDepthTexture
    , saveTexture
    ) where

import Codec.Picture
import Codec.Picture.Types
import Control.Arrow
import Control.Monad
import Data.Maybe
import qualified Data.Vector.Storable as VS
import qualified Foreign as F
import Graphics.Rendering.OpenGL
import System.Log.Logger

import Debug
import FunctionalGL
import Heightmap
import Misc

----------------------------------------------------------------------------------------------------

acquireImage :: String -> ResourceIO (Maybe (TextureObject, ResourceIO ()))
acquireImage fileName = do
    texture <- acquireResourceWith fileName (fst . fromJust <$> loadImage fileName)
    return (Just (texture, releaseResource fileName))

----------------------------------------------------------------------------------------------------

loadImage :: String -> IO (Maybe (TextureObject, Dispose))
loadImage path = withLoadedImage path $ \t -> do
    generateMipmap' Texture2D
    --
    textureWrapMode Texture2D S $= (Repeated, Repeat)
    textureWrapMode Texture2D T $= (Repeated, Repeat)
    textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
    -- textureFilter Texture2D $= ((Linear', Nothing), Linear')
    --

withLoadedImage :: String -> (TextureObject -> IO ()) -> IO (Maybe (TextureObject, Dispose))
withLoadedImage path action = do
    image <- readImage path -- :: IO (Either String (Image RP RGB Word8))

    let loadPixels format width height content = do
            texture <- genObjectName
            withTexture2D texture $ do
                VS.unsafeWith content $ \ptr ->
                    texImage2D
                        Texture2D
                        NoProxy
                        0 -- The mipmap level this image is responsible for.
                        RGBA8
                        (TextureSize2D (fromIntegral width) (fromIntegral height))
                        0 -- No borders
                        (PixelData format UnsignedByte ptr)
                action texture
                return (Just (texture, deleteObjectName texture))

    case image of
        Left e -> do
            errorM "Kage" ("could not load image " ++ path ++ ": " ++ e)
            return Nothing
        Right dynamicImage -> do
            infoM "Kage" ("Loading image format " ++ path ++ ": " ++ fst (getFormatName dynamicImage))
            case dynamicImage of
                ImageY8 (Image width height content) -> loadPixels Red width height content
                ImageRGB8 (Image width height content) -> loadPixels RGB width height content
                ImageRGBA8 (Image width height content) -> loadPixels RGBA width height content
                ImageYCbCr8 image ->
                    let (Image width height content) = convertImage image :: Image PixelRGB8
                    in  loadPixels RGB width height content
                _ -> do
                    errorM "Kage" ("Unmanaged image format " ++ path ++ ": " ++ fst (getFormatName dynamicImage))
                    return Nothing

loadImageR8 :: String -> IO (Maybe (TextureObject, Dispose))
loadImageR8 path = withLoadedImageR8 path $ \t -> do
    generateMipmap' Texture2D
    --
    textureWrapMode Texture2D S $= (Repeated, Repeat)
    textureWrapMode Texture2D T $= (Repeated, Repeat)
    textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
    -- textureFilter Texture2D $= ((Linear', Nothing), Linear')
    --
    return t

withLoadedImageR8 :: String -> (TextureObject -> IO a) -> IO (Maybe (a, Dispose))
withLoadedImageR8 path action = do
    image <- readImage path -- :: IO (Either String (Image RP RGB Word8))

    let loadPixels format width height content = do
            texture <- genObjectName
            withTexture2D texture $ do
                VS.unsafeWith content $ \ptr ->
                    texImage2D
                        Texture2D
                        NoProxy
                        0 -- The mipmap level this image is responsible for.
                        R8
                        (TextureSize2D (fromIntegral width) (fromIntegral height))
                        0 -- No borders
                        (PixelData format UnsignedByte ptr)
                result <- action texture
                return (Just (result, deleteObjectName texture))

    case image of
        Left e -> do
            errorM "Kage" ("could not load image " ++ path ++ ": " ++ e)
            return Nothing
        Right dynamicImage -> case dynamicImage of
            ImageRGB8 (Image width height content) -> loadPixels RGB width height content
            ImageRGBA8 (Image width height content) -> loadPixels RGBA width height content
            ImageYCbCr8 image ->
                let (Image width height content) = convertImage image :: Image PixelRGB8
                in  loadPixels RGB width height content
            _ -> do
                errorM "Kage" ("Unmanaged image format " ++ path ++ ": " ++ fst (getFormatName dynamicImage))
                return Nothing

loadHeightmap :: String -> IO (Heightmap Float)
loadHeightmap path = do
    let average v = sum (VS.toList v) `divR` VS.length v
        greatest v = fromIntegral (maximum (VS.toList v))
        compact f n v = if VS.null v then Nothing else Just (first f (VS.splitAt n v))

    result <- readImage path
    Just (width, height, values) <- case result of
        Left e -> return Nothing
        Right dynamicImage -> case dynamicImage of
            ImageY8 (Image width height content) ->
                return (Just (width, height, VS.map fromIntegral content))
            ImageRGB8 (Image width height content) ->
                return (Just (width, height, VS.unfoldr (compact greatest 3) content))
            _ -> do
                errorM "Kage" ("Unmanaged heightmap format: " ++ fst (getFormatName dynamicImage))
                return Nothing
    return (width, height, values)

saveDepthTexture :: (Int, Int) -> TextureObject -> String -> IO ()
saveDepthTexture (w, h) texture path =
    withTexture2D texture $ do
        let depth = F.sizeOf (0 :: Float)
        F.allocaBytes (depth * w * h) $ \ptr -> do
            getTexImage Texture2D 0 (PixelData DepthComponent Float ptr)
            let readPixel x y = do
                    d <- F.peek (F.plusPtr ptr ((x + (h - y) * w) * depth)) :: IO Float
                    return $ round (clamp 0 255 (255 * d))
            image <- withImage w h readPixel :: IO (Image Pixel8)
            savePngImage path (ImageY8 image)
            infoM "Kage" "Shadowmap captured"

saveTexture :: (Int, Int) -> TextureObject -> String -> IO ()
saveTexture (w, h) texture path =
    withTexture2D texture $ do
        packRowAlg <- fromIntegral <$> get (rowAlignment Pack)
        let depth = F.sizeOf (0 :: F.Word8)
            dw = ((w + packRowAlg - 1) `div` packRowAlg) * packRowAlg - w
        F.allocaBytes (depth * (w + dw) * h) $ \ptr -> do
            getTexImage Texture2D 0 (PixelData Red UnsignedByte ptr)
            let readPixel x y = F.peek (F.plusPtr ptr ((x + y * (w + dw)) * depth)) :: IO F.Word8
            image <- withImage w h readPixel :: IO (Image Pixel8)
            savePngImage path (ImageY8 image)

getFormatName :: DynamicImage -> (String, String)
getFormatName dynamicImage = case dynamicImage of
    ImageY8 _       -> ("Y8",       "a greyscale image")
    ImageY16 _      -> ("Y16",      "a greyscale image with 16bit components")
    ImageYF _       -> ("YF",       "a greyscale HDR image")
    ImageYA8 _      -> ("YA8",      "an image in greyscale with an alpha channel")
    ImageYA16 _     -> ("YA16",     "an image in greyscale with alpha channel on 16 bits")
    ImageRGB8 _     -> ("RGB8",     "an image in true color")
    ImageRGB16 _    -> ("RGB16",    "an image in true color with 16bit depth")
    ImageRGBF _     -> ("RGBF",     "an image with HDR pixels")
    ImageRGBA8 _    -> ("RGBA8",    "an image in true color and an alpha channel")
    ImageRGBA16 _   -> ("RGBA16",   "a true color image with alpha on 16 bits")
    ImageYCbCr8 _   -> ("YCbCr8",   "an image in the colorspace used by Jpeg images")
    ImageCMYK8 _    -> ("CMYK8",    "an image in the colorspace CMYK")
    ImageCMYK16 _   -> ("CMYK16",   "an image in the colorspace CMYK and 16 bits precision")
