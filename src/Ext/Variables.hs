module Ext.Variables (
    VariableType(..), unmarshalVariableType, activeVars
) where

import Control.Monad
import Data.StateVar
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL

import Graphics.Rendering.OpenGL (unpackUtf8)

import Ext.Program
import Ext.Utils

--------------------------------------------------------------------------------

-- Table 2.9 of the OpenGL 3.1 spec: OpenGL Shading Language type tokens
data VariableType =
     Float'
   | FloatVec2
   | FloatVec3
   | FloatVec4
   | Int'
   | IntVec2
   | IntVec3
   | IntVec4
   | UnsignedInt'
   | UnsignedIntVec2
   | UnsignedIntVec3
   | UnsignedIntVec4
   | Bool
   | BoolVec2
   | BoolVec3
   | BoolVec4
   | FloatMat2
   | FloatMat3
   | FloatMat4
   | FloatMat2x3
   | FloatMat2x4
   | FloatMat3x2
   | FloatMat3x4
   | FloatMat4x2
   | FloatMat4x3
   | Sampler1D
   | Sampler2D
   | Sampler3D
   | SamplerCube
   | Sampler1DShadow
   | Sampler2DShadow
   | Sampler1DArray
   | Sampler2DArray
   | Sampler1DArrayShadow
   | Sampler2DArrayShadow
   | SamplerCubeShadow
   | Sampler2DRect
   | Sampler2DRectShadow
   | IntSampler1D
   | IntSampler2D
   | IntSampler3D
   | IntSamplerCube
   | IntSampler1DArray
   | IntSampler2DArray
   | UnsignedIntSampler1D
   | UnsignedIntSampler2D
   | UnsignedIntSampler3D
   | UnsignedIntSamplerCube
   | UnsignedIntSampler1DArray
   | UnsignedIntSampler2DArray
   deriving ( Eq, Ord, Show )

unmarshalVariableType :: GLenum -> VariableType
unmarshalVariableType x
   | x == GL_FLOAT = Float'
   | x == GL_FLOAT_VEC2 = FloatVec2
   | x == GL_FLOAT_VEC3 = FloatVec3
   | x == GL_FLOAT_VEC4 = FloatVec4
   | x == GL_INT = Int'
   | x == GL_INT_VEC2 = IntVec2
   | x == GL_INT_VEC3 = IntVec3
   | x == GL_INT_VEC4 = IntVec4
   | x == GL_UNSIGNED_INT = UnsignedInt'
   | x == GL_UNSIGNED_INT_VEC2 = UnsignedIntVec2
   | x == GL_UNSIGNED_INT_VEC3 = UnsignedIntVec3
   | x == GL_UNSIGNED_INT_VEC4 = UnsignedIntVec4
   | x == GL_BOOL = Bool
   | x == GL_BOOL_VEC2 = BoolVec2
   | x == GL_BOOL_VEC3 = BoolVec3
   | x == GL_BOOL_VEC4 = BoolVec4
   | x == GL_FLOAT_MAT2 = FloatMat2
   | x == GL_FLOAT_MAT3 = FloatMat3
   | x == GL_FLOAT_MAT4 = FloatMat4
   | x == GL_FLOAT_MAT2x3 = FloatMat2x3
   | x == GL_FLOAT_MAT2x4 = FloatMat2x4
   | x == GL_FLOAT_MAT3x2 = FloatMat3x2
   | x == GL_FLOAT_MAT3x4 = FloatMat3x4
   | x == GL_FLOAT_MAT4x2 = FloatMat4x2
   | x == GL_FLOAT_MAT4x3 = FloatMat4x3
   | x == GL_SAMPLER_1D = Sampler1D
   | x == GL_SAMPLER_2D = Sampler2D
   | x == GL_SAMPLER_3D = Sampler3D
   | x == GL_SAMPLER_CUBE = SamplerCube
   | x == GL_SAMPLER_1D_SHADOW = Sampler1DShadow
   | x == GL_SAMPLER_2D_SHADOW = Sampler2DShadow
   | x == GL_SAMPLER_1D_ARRAY = Sampler1DArray
   | x == GL_SAMPLER_2D_ARRAY = Sampler2DArray
   | x == GL_SAMPLER_1D_ARRAY_SHADOW = Sampler1DArrayShadow
   | x == GL_SAMPLER_2D_ARRAY_SHADOW = Sampler2DArrayShadow
   | x == GL_SAMPLER_CUBE_SHADOW = SamplerCubeShadow
   | x == GL_SAMPLER_2D_RECT = Sampler2DRect
   | x == GL_SAMPLER_2D_RECT_SHADOW = Sampler2DRectShadow
   | x == GL_INT_SAMPLER_1D = IntSampler1D
   | x == GL_INT_SAMPLER_2D = IntSampler2D
   | x == GL_INT_SAMPLER_3D = IntSampler3D
   | x == GL_INT_SAMPLER_CUBE = IntSamplerCube
   | x == GL_INT_SAMPLER_1D_ARRAY = IntSampler1DArray
   | x == GL_INT_SAMPLER_2D_ARRAY = IntSampler2DArray
   | x == GL_UNSIGNED_INT_SAMPLER_1D = UnsignedIntSampler1D
   | x == GL_UNSIGNED_INT_SAMPLER_2D = UnsignedIntSampler2D
   | x == GL_UNSIGNED_INT_SAMPLER_3D = UnsignedIntSampler3D
   | x == GL_UNSIGNED_INT_SAMPLER_CUBE = UnsignedIntSamplerCube
   | x == GL_UNSIGNED_INT_SAMPLER_1D_ARRAY = UnsignedIntSampler1DArray
   | x == GL_UNSIGNED_INT_SAMPLER_2D_ARRAY = UnsignedIntSampler2DArray
   | otherwise = error ("unmarshalVariableType: illegal value " ++ show x)

--------------------------------------------------------------------------------

activeVars :: (ExtProgram -> GettableStateVar GLuint)
           -> (ExtProgram -> GettableStateVar GLsizei)
           -> (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> IO ())
           -> (GLenum -> a)
           -> ExtProgram
           -> GettableStateVar [(GLint,a,String)]
activeVars numVars maxLength getter unmarshalType p@(ExtProgram program) =
   makeGettableStateVar $ do
      numActiveVars <- get (numVars p)
      maxLen <- get (maxLength p)
      with 0 $ \nameLengthBuf ->
         with 0 $ \sizeBuf ->
            with 0 $ \typeBuf ->
               let ixs = if numActiveVars > 0 then [0 .. numActiveVars-1] else []
               in forM ixs $ \i -> do
                  n <- createAndTrimByteString maxLen $ \nameBuf -> do
                     getter program i maxLen nameLengthBuf sizeBuf typeBuf nameBuf
                     peek nameLengthBuf
                  s <- peek1 fromIntegral sizeBuf
                  t <- peek1 unmarshalType typeBuf
                  return (s, t, unpackUtf8 n)
