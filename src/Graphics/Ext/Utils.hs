module Graphics.Ext.Utils
    (   marshalGLboolean
    ,   unmarshalGLboolean
    ,   stringQuery
    ,   createByteString
    ,   createAndTrimByteString
    ,   withByteString
    ,   withGLstring
    ,   peek1
    ,   peek3
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Foreign (allocaArray, alloca, peek, plusPtr, withArray, peekArray, nullPtr, Ptr, castPtr)
import Foreign.Storable
import Graphics.Rendering.OpenGL
import qualified Graphics.GL as Raw

----------------------------------------------------------------------------------------------------

marshalGLboolean :: Num a => Bool -> a
marshalGLboolean x = fromIntegral $ if x then Raw.GL_TRUE else Raw.GL_FALSE

unmarshalGLboolean :: (Eq a, Num a) => a -> Bool
unmarshalGLboolean = (/= fromIntegral Raw.GL_FALSE)

----------------------------------------------------------------------------------------------------

{-# INLINE peek1 #-}
peek1 :: Storable a => (a -> b) -> Ptr a -> IO b
peek1 f ptr = do
    x <- peekElemOff ptr 0
    return $ f x

{-# INLINE peek3 #-}
peek3 :: Storable a => (a -> a -> a -> b) -> Ptr a -> IO b
peek3 f = peek3M $ \x y z -> return (f x y z)

{-# INLINE peek3M #-}
peek3M :: Storable a => (a -> a -> a -> IO b) -> Ptr a -> IO b
peek3M f ptr = do
    x <- peekElemOff ptr 0
    y <- peekElemOff ptr 1
    z <- peekElemOff ptr 2
    f x y z

----------------------------------------------------------------------------------------------------

stringQuery :: (a -> GettableStateVar GLsizei)
            -> (a -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
            -> a
            -> IO BS.ByteString
stringQuery lengthVar getStr obj = do
    len <- get (lengthVar obj)
    createByteString len $
        getStr obj len nullPtr

createByteString :: Integral a => a -> (Ptr GLchar -> IO ()) -> IO BS.ByteString
createByteString size act = BI.create (fromIntegral size) (act . castPtr)

createAndTrimByteString ::
    (Integral a, Integral b) => a -> (Ptr GLchar -> IO b) -> IO BS.ByteString
createAndTrimByteString maxLen act =
    BI.createAndTrim (fromIntegral maxLen) (fmap fromIntegral . act . castPtr)

withByteString :: BS.ByteString -> (Ptr GLchar -> GLsizei -> IO b) -> IO b
withByteString bs act =
    BU.unsafeUseAsCStringLen bs $ \(ptr, size) ->
    act (castPtr ptr) (fromIntegral size)

withGLstring :: String -> (Ptr GLchar -> IO a) -> IO a
withGLstring s act = withByteString (packUtf8 (s ++ "\0")) (const . act)

----------------------------------------------------------------------------------------------------

class GetPName p where
    marshalGetPName :: p -> Maybe GLenum

getIntegerv :: GetPName p => p -> Ptr GLint -> IO ()
getIntegerv = makeGetter Raw.glGetIntegerv

{-# INLINE makeGetter #-}
makeGetter :: GetPName p => (Raw.GLenum -> Ptr a -> IO ()) -> p -> Ptr a -> IO ()
-- makeGetter f = maybe (const recordInvalidEnum) f . marshalGetPName
makeGetter f = maybe (error "DTC") f . marshalGetPName

-- | Helper function for the get*1 functions.
get1 :: (Storable b, Storable c)
    => (p -> Ptr c -> IO ())
    -> (b -> a) -- ^ Conversion from the casted value to the return value
    -> p -> IO a
get1 g f n = alloca $ \buf -> do
    g n buf
    peek1 f (castPtr buf)
