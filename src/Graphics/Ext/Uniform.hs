module Graphics.Ext.Uniform
    (   extActiveUniforms
    ,   extUniformLocation
    ) where

import Data.Maybe
import Data.StateVar
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL
import Graphics.Rendering.OpenGL.GL.Shaders.Uniform

import Graphics.Ext.Program
import Graphics.Ext.Variables
import Graphics.Ext.Utils

--------------------------------------------------------------------------------

extNumActiveUniforms :: ExtProgram -> GettableStateVar GLuint
extNumActiveUniforms = extProgramVar1 fromIntegral ActiveUniforms

extActiveUniformMaxLength :: ExtProgram -> GettableStateVar GLsizei
extActiveUniformMaxLength = extProgramVar1 fromIntegral ActiveUniformMaxLength

--------------------------------------------------------------------------------

extUniformLocation :: ExtProgram -> String -> GettableStateVar UniformLocation
extUniformLocation (ExtProgram program) name =
   makeGettableStateVar $
      fmap UniformLocation $
         withGLstring name $
            glGetUniformLocation program

--------------------------------------------------------------------------------

extActiveUniforms :: ExtProgram -> GettableStateVar [(GLint, VariableType, String)]
extActiveUniforms =
   activeVars
      extNumActiveUniforms
      extActiveUniformMaxLength
      glGetActiveUniform
      unmarshalVariableType
