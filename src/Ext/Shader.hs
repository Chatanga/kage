module Ext.Shader
    (   ExtShader(..)
    ,   createExtShader
    ,   extShaderSourceBS
    ,   getExtShaderSource
    ,   extShaderSourceLength
    ,   setExtShaderSource
    ,   compileExtShader
    ,   releaseExtShaderCompiler
    ,   extShaderType
    ,   extShaderDeleteStatus
    ,   extCompileStatus
    ,   extShaderInfoLog
    ,   extShaderInfoLogLength
    ) where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Foreign.Marshal.Utils (with)
import Graphics.Rendering.OpenGL
import qualified Graphics.GL as Raw

import Ext.Utils

----------------------------------------------------------------------------------------------------

newtype ExtShader = ExtShader { shaderID :: GLuint }
   deriving ( Eq, Ord, Show )

instance ObjectName ExtShader where
   isObjectName = liftIO . fmap unmarshalGLboolean . Raw.glIsShader . shaderID
   deleteObjectName = liftIO . Raw.glDeleteShader . shaderID

----------------------------------------------------------------------------------------------------

createExtShader :: ShaderType -> IO ExtShader
createExtShader = fmap ExtShader . Raw.glCreateShader . marshalShaderType

--------------------------------------------------------------------------------

-- | UTF8 encoded.
extShaderSourceBS :: ExtShader -> StateVar BS.ByteString
extShaderSourceBS shader =
   makeStateVar (getExtShaderSource shader) (setExtShaderSource shader)

getExtShaderSource :: ExtShader -> IO BS.ByteString
getExtShaderSource = stringQuery extShaderSourceLength (Raw.glGetShaderSource . shaderID)

extShaderSourceLength :: ExtShader -> GettableStateVar GLsizei
extShaderSourceLength = shaderVar fromIntegral ShaderSourceLength

setExtShaderSource :: ExtShader -> BS.ByteString -> IO ()
setExtShaderSource shader src =
   withByteString src $ \srcPtr srcLength ->
      with srcPtr $ \srcPtrBuf ->
         with srcLength $ \srcLengthBuf ->
            Raw.glShaderSource (shaderID shader) 1 srcPtrBuf srcLengthBuf

--------------------------------------------------------------------------------

marshalShaderType :: ShaderType -> GLenum
marshalShaderType x = case x of
   VertexShader -> Raw.GL_VERTEX_SHADER
   TessControlShader -> Raw.GL_TESS_CONTROL_SHADER
   TessEvaluationShader -> Raw.GL_TESS_EVALUATION_SHADER
   GeometryShader -> Raw.GL_GEOMETRY_SHADER
   FragmentShader -> Raw.GL_FRAGMENT_SHADER
   ComputeShader -> Raw.GL_COMPUTE_SHADER

unmarshalShaderType :: GLenum -> ShaderType
unmarshalShaderType x
   | x == Raw.GL_VERTEX_SHADER = VertexShader
   | x == Raw.GL_TESS_CONTROL_SHADER = TessControlShader
   | x == Raw.GL_TESS_EVALUATION_SHADER = TessEvaluationShader
   | x == Raw.GL_GEOMETRY_SHADER = GeometryShader
   | x == Raw.GL_FRAGMENT_SHADER = FragmentShader
   | x == Raw.GL_COMPUTE_SHADER = ComputeShader
   | otherwise = error ("unmarshalShaderType: illegal value " ++ show x)

--------------------------------------------------------------------------------

compileExtShader :: ExtShader -> IO ()
compileExtShader = Raw.glCompileShader . shaderID

releaseExtShaderCompiler :: IO ()
releaseExtShaderCompiler = Raw.glReleaseShaderCompiler

--------------------------------------------------------------------------------

extShaderType :: ExtShader -> GettableStateVar ShaderType
extShaderType = shaderVar (unmarshalShaderType . fromIntegral) ShaderType

extShaderDeleteStatus :: ExtShader -> GettableStateVar Bool
extShaderDeleteStatus = shaderVar unmarshalGLboolean ShaderDeleteStatus

extCompileStatus :: ExtShader -> GettableStateVar Bool
extCompileStatus = shaderVar unmarshalGLboolean CompileStatus

extShaderInfoLog :: ExtShader -> GettableStateVar String
extShaderInfoLog =
   makeGettableStateVar .
      fmap unpackUtf8 .
         stringQuery extShaderInfoLogLength (Raw.glGetShaderInfoLog . shaderID)

extShaderInfoLogLength :: ExtShader -> GettableStateVar GLsizei
extShaderInfoLogLength = shaderVar fromIntegral ShaderInfoLogLength

--------------------------------------------------------------------------------

data GetShaderPName =
     ShaderDeleteStatus
   | CompileStatus
   | ShaderInfoLogLength
   | ShaderSourceLength
   | ShaderType

marshalGetShaderPName :: GetShaderPName -> GLenum
marshalGetShaderPName x = case x of
   ShaderDeleteStatus -> Raw.GL_DELETE_STATUS
   CompileStatus -> Raw.GL_COMPILE_STATUS
   ShaderInfoLogLength -> Raw.GL_INFO_LOG_LENGTH
   ShaderSourceLength -> Raw.GL_SHADER_SOURCE_LENGTH
   ShaderType -> Raw.GL_SHADER_TYPE

shaderVar :: (GLint -> a) -> GetShaderPName -> ExtShader -> GettableStateVar a
shaderVar f p shader =
   makeGettableStateVar $
      with 0 $ \buf -> do
         Raw.glGetShaderiv (shaderID shader) (marshalGetShaderPName p) buf
         peek1 f buf
