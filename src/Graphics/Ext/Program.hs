module Graphics.Ext.Program
    (   ExtProgram(..)
    ,   GetProgramPName(..)
    ,   extProgramVar1
    ,   extProgramVar3
    ,   extProgramVarN
    ,   createExtProgram
    ,   attachExtShader
    ,   detachExtShader
    ,   attachedExtShaders
    ,   getAttachedExtShaders
    ,   setAttachedExtShaders
    ,   linkExtProgram
    ,   currentExtProgram
    ,   validateExtProgram
    ,   extProgramInfoLog
    ,   extProgramSeparable
    ,   extProgramBinaryRetrievableHint
    ,   extProgramStateVarBool
    ,   extProgramDeleteStatus
    ,   extLinkStatus
    ,   extValidateStatus
    ,   extProgramInfoLogLength
    ,   numAttachedExtShaders
    ,   extBindFragDataLocation
    ,   extGetFragDataLocation
    ) where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.List
import Data.Maybe
import Foreign.Marshal.Utils (with)
import Foreign (allocaArray, alloca, peek, plusPtr, withArray, peekArray, nullPtr, Ptr, castPtr)
import Foreign.Storable
import Graphics.Rendering.OpenGL
import qualified Graphics.GL as Raw

import Graphics.Ext.Shader
import Graphics.Ext.Utils

----------------------------------------------------------------------------------------------------

newtype ExtProgram = ExtProgram { programID :: GLuint }
   deriving ( Eq, Ord, Show )

instance ObjectName ExtProgram where
   isObjectName = liftIO . fmap unmarshalGLboolean . Raw.glIsProgram . programID
   deleteObjectName = liftIO . Raw.glDeleteProgram . programID

data GetProgramPName =
     ProgramDeleteStatus
   | LinkStatus
   | ValidateStatus
   | ProgramInfoLogLength
   | AttachedShaders
   | ActiveAttributes
   | ActiveAttributeMaxLength
   | ActiveUniforms
   | ActiveUniformMaxLength
   | TransformFeedbackBufferMode
   | TransformFeedbackVaryings
   | TransformFeedbackVaryingMaxLength
   | ActiveUniformBlocks
   | ActiveUniformBlockMaxNameLength
   | GeometryVerticesOut
   | GeometryInputType
   | GeometryOutputType
   | GeometryShaderInvocations
   | TessControlOutputVertices
   | TessGenMode
   | TessGenSpacing
   | TessGenVertexOrder
   | TessGenPointMode
   | ComputeWorkGroupSize  -- 3 integers!
   | ProgramSeparable
   | ProgramBinaryRetrievableHint
   | ActiveAtomicCounterBuffers
   | ProgramBinaryLength

marshalGetProgramPName :: GetProgramPName -> GLenum
marshalGetProgramPName x = case x of
   ProgramDeleteStatus -> Raw.GL_DELETE_STATUS
   LinkStatus -> Raw.GL_LINK_STATUS
   ValidateStatus -> Raw.GL_VALIDATE_STATUS
   ProgramInfoLogLength -> Raw.GL_INFO_LOG_LENGTH
   AttachedShaders -> Raw.GL_ATTACHED_SHADERS
   ActiveAttributes -> Raw.GL_ACTIVE_ATTRIBUTES
   ActiveAttributeMaxLength -> Raw.GL_ACTIVE_ATTRIBUTE_MAX_LENGTH
   ActiveUniforms -> Raw.GL_ACTIVE_UNIFORMS
   ActiveUniformMaxLength -> Raw.GL_ACTIVE_UNIFORM_MAX_LENGTH
   TransformFeedbackBufferMode -> Raw.GL_TRANSFORM_FEEDBACK_BUFFER_MODE
   TransformFeedbackVaryings -> Raw.GL_TRANSFORM_FEEDBACK_VARYINGS
   TransformFeedbackVaryingMaxLength -> Raw.GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH
   ActiveUniformBlocks -> Raw.GL_ACTIVE_UNIFORM_BLOCKS
   ActiveUniformBlockMaxNameLength -> Raw.GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH
   GeometryVerticesOut -> Raw.GL_GEOMETRY_VERTICES_OUT
   GeometryInputType -> Raw.GL_GEOMETRY_INPUT_TYPE
   GeometryOutputType -> Raw.GL_GEOMETRY_OUTPUT_TYPE
   GeometryShaderInvocations -> Raw.GL_GEOMETRY_SHADER_INVOCATIONS
   TessControlOutputVertices -> Raw.GL_TESS_CONTROL_OUTPUT_VERTICES
   TessGenMode -> Raw.GL_TESS_GEN_MODE
   TessGenSpacing -> Raw.GL_TESS_GEN_SPACING
   TessGenVertexOrder -> Raw.GL_TESS_GEN_VERTEX_ORDER
   TessGenPointMode -> Raw.GL_TESS_GEN_POINT_MODE
   ComputeWorkGroupSize -> Raw.GL_COMPUTE_WORK_GROUP_SIZE
   ProgramSeparable -> Raw.GL_PROGRAM_SEPARABLE
   ProgramBinaryRetrievableHint -> Raw.GL_PROGRAM_BINARY_RETRIEVABLE_HINT
   ActiveAtomicCounterBuffers -> Raw.GL_ACTIVE_ATOMIC_COUNTER_BUFFERS
   ProgramBinaryLength -> Raw.GL_PROGRAM_BINARY_LENGTH

extProgramVar1 :: (GLint -> a) -> GetProgramPName -> ExtProgram -> GettableStateVar a
extProgramVar1 = extProgramVarN . peek1

extProgramVar3 :: (GLint -> GLint -> GLint -> a) -> GetProgramPName -> ExtProgram -> GettableStateVar a
extProgramVar3 = extProgramVarN . peek3

extProgramVarN :: (Ptr GLint -> IO a) -> GetProgramPName -> ExtProgram -> GettableStateVar a
extProgramVarN f p program =
    makeGettableStateVar $
        with 0 $ \buf -> do
            Raw.glGetProgramiv (programID program) (marshalGetProgramPName p) buf
            f buf

createExtProgram :: IO ExtProgram
createExtProgram = fmap ExtProgram Raw.glCreateProgram

--------------------------------------------------------------------------------

attachExtShader :: ExtProgram -> ExtShader -> IO ()
attachExtShader p s = Raw.glAttachShader (programID p) (shaderID s)

detachExtShader :: ExtProgram -> ExtShader -> IO ()
detachExtShader p s = Raw.glDetachShader (programID p) (shaderID s)

attachedExtShaders :: ExtProgram -> StateVar [ExtShader]
attachedExtShaders program =
   makeStateVar (getAttachedExtShaders program) (setAttachedExtShaders program)

getAttachedExtShaders :: ExtProgram -> IO [ExtShader]
getAttachedExtShaders program = do
   numShaders <- get (numAttachedExtShaders program)
   ids <- allocaArray (fromIntegral numShaders) $ \buf -> do
      Raw.glGetAttachedShaders (programID program) numShaders nullPtr buf
      peekArray (fromIntegral numShaders) buf
   return $ map ExtShader ids

setAttachedExtShaders :: ExtProgram -> [ExtShader] -> IO ()
setAttachedExtShaders program newShaders = do
   currentShaders <- getAttachedExtShaders program
   mapM_ (attachExtShader program) (newShaders \\ currentShaders)
   mapM_ (detachExtShader program) (currentShaders \\ newShaders)

--------------------------------------------------------------------------------

linkExtProgram :: ExtProgram -> IO ()
linkExtProgram = Raw.glLinkProgram . programID

{-
currentExtProgram :: StateVar (Maybe ExtProgram)
currentExtProgram =
   makeStateVar
      (do p <- fmap ExtProgram $ getInteger1 fromIntegral GetCurrentProgram
          return $ if p == noExtProgram then Nothing else Just p)
      (Raw.glUseProgram . programID . fromMaybe noExtProgram)
-}

currentExtProgram :: StateVar (Maybe ExtProgram)
currentExtProgram =
    makeStateVar
        (do p <- alloca $ \ptr -> do
                Raw.glGetProgramiv 1 Raw.GL_CURRENT_PROGRAM ptr
                ExtProgram . fromIntegral <$> peek ptr
            return $ if p == noExtProgram then Nothing else Just p)
        (Raw.glUseProgram . programID . fromMaybe noExtProgram)

noExtProgram :: ExtProgram
noExtProgram = ExtProgram 0

validateExtProgram :: ExtProgram -> IO ()
validateExtProgram = Raw.glValidateProgram . programID

extProgramInfoLog :: ExtProgram -> GettableStateVar String
extProgramInfoLog =
   makeGettableStateVar .
      fmap unpackUtf8 .
         stringQuery extProgramInfoLogLength (Raw.glGetProgramInfoLog . programID)

--------------------------------------------------------------------------------

extProgramSeparable :: ExtProgram -> StateVar Bool
extProgramSeparable = extProgramStateVarBool ProgramSeparable

extProgramBinaryRetrievableHint :: ExtProgram -> StateVar Bool
extProgramBinaryRetrievableHint = extProgramStateVarBool ProgramBinaryRetrievableHint

extProgramStateVarBool :: GetProgramPName -> ExtProgram -> StateVar Bool
extProgramStateVarBool pname program =
   makeStateVar
      (get (extProgramVar1 unmarshalGLboolean pname program))
      (Raw.glProgramParameteri (programID program)
                           (marshalGetProgramPName pname) . marshalGLboolean)

--------------------------------------------------------------------------------

extProgramDeleteStatus :: ExtProgram -> GettableStateVar Bool
extProgramDeleteStatus = extProgramVar1 unmarshalGLboolean ProgramDeleteStatus

extLinkStatus :: ExtProgram -> GettableStateVar Bool
extLinkStatus = extProgramVar1 unmarshalGLboolean LinkStatus

extValidateStatus :: ExtProgram -> GettableStateVar Bool
extValidateStatus = extProgramVar1 unmarshalGLboolean ValidateStatus

extProgramInfoLogLength :: ExtProgram -> GettableStateVar GLsizei
extProgramInfoLogLength = extProgramVar1 fromIntegral ProgramInfoLogLength

numAttachedExtShaders :: ExtProgram -> GettableStateVar GLsizei
numAttachedExtShaders = extProgramVar1 fromIntegral AttachedShaders

--------------------------------------------------------------------------------

-- | 'bindFragDataLocation' binds a varying variable, specified by program and name, to a
-- drawbuffer. The effects only take place after succesfull linking of the program.
-- invalid arguments and conditions are
-- - an index larger than maxDrawBufferIndex
-- - names starting with 'gl_'
-- linking failure will ocure when
-- - one of the arguments was invalid
-- - more than one varying varuable name is bound to the same index
-- It's not an error to specify unused variables, those will be ingored.
extBindFragDataLocation :: ExtProgram -> String -> SettableStateVar DrawBufferIndex
extBindFragDataLocation (ExtProgram program) varName = makeSettableStateVar $ \ind ->
   withGLstring varName $ Raw.glBindFragDataLocation program ind

-- | query the binding of a given variable, specified by program and name. The program has to be
-- linked. The result is Nothing if an error occures or the name is not a name of a varying
-- variable. If the program hasn't been linked an 'InvalidOperation' error is generated.
extGetFragDataLocation :: ExtProgram -> String -> IO (Maybe DrawBufferIndex)
extGetFragDataLocation (ExtProgram program) varName = do
    r <- withGLstring varName $ Raw.glGetFragDataLocation program
    if r < 0
        then return Nothing
        else return . Just $ fromIntegral r
