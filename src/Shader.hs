module Shader
    (   createProgramWithShaders'
    ,   createProgramWithShaders
    ) where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Char
import Data.List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Foreign (allocaArray, alloca, peek, plusPtr, withArray, peekArray, nullPtr, Ptr, castPtr)
import Graphics.Rendering.OpenGL
import qualified Graphics.GL as Raw
import System.FilePath
import System.Log.Logger
import qualified Text.Regex as RE

import Debug
import Ext.Program
import Ext.Shader
import FunctionalGL
import Misc

----------------------------------------------------------------------------------------------------

createProgramWithShaders' :: String -> String -> IO (ExtProgram, Dispose)
createProgramWithShaders' vertexShaderName fragmentShaderName = createProgramWithShaders
    [ (vertexShaderName, VertexShader)
    , (fragmentShaderName, FragmentShader)
    ]

createProgramWithShaders :: [(String, ShaderType)] -> IO (ExtProgram, Dispose)
createProgramWithShaders shaderSources = do
    program <- createExtProgram

    shaders <- forM shaderSources $ \(shaderName, shaderType) -> do
        infoM "Kage" ("Loading shader " ++ shaderName ++ " as " ++ show shaderType)
        shader <- createExtShader shaderType
        vs <- readFileContentWithImports ("shaders" </> shaderName)
        extShaderSourceBS shader $= vs
        compileOk <- compileShader' shaderName shader
        when compileOk $ attachExtShader program shader
        return shader

    {- In VS:
    attribLocation program "inPosition" $= AttribLocation 0
    attribLocation program "inColor" $= AttribLocation 1
    attribLocation program "inTexCoord" $= AttribLocation 2
    attribLocation program "inNormal" $= AttribLocation 3
    -}

    {- In FS:
    bindFragDataLocation program "gPosition" $= 0
    bindFragDataLocation program "gNormal" $= 1
    bindFragDataLocation program "gAlbedoSpec" $= 2
    -}
    linkProgram' program

    -- Delete the shaders as the program has them now.
    deleteObjectNames shaders

    return (program, deleteObjectName program)

createPipelineWithShaders :: [(String, ShaderType)] -> IO (ExtProgram, Dispose)
createPipelineWithShaders shaderSources = do
    program <- createExtProgram

    shaders <- forM shaderSources $ \(shaderName, shaderType) -> do
        infoM "Kage" ("Loading shader " ++ shaderName ++ " as " ++ show shaderType)
        shader <- createExtShader shaderType
        vs <- readFileContentWithImports ("shaders" </> shaderName)
        extShaderSourceBS shader $= vs
        compileOk <- compileShader' shaderName shader
        when compileOk $ attachExtShader program shader
        return shader

    linkProgram' program

    -- Delete the shaders as the program has them now.
    deleteObjectNames shaders

    extProgramSeparable program $= True

    -- The program pipeline represents the collection of programs in use:
    -- Generate the name for it here.
    pipeline <- alloca $ \ptr -> do
        Raw.glCreateProgramPipelines 1 ptr
        peek ptr

    let (ExtProgram pid) = program
    -- Now use the vertex shader from the first program and the fragment
    -- shader from the second program.
    Raw.glUseProgramStages pipeline Raw.GL_VERTEX_SHADER_BIT pid;
    Raw.glUseProgramStages pipeline Raw.GL_FRAGMENT_SHADER_BIT pid;

    return (program, deleteObjectName program)

compileShader' shaderName shader = do
    compileExtShader shader
    checkShaderCompilation shaderName shader

checkShaderCompilation :: String -> ExtShader -> IO Bool
checkShaderCompilation shaderName shader = do
    compileOk <- get (extCompileStatus shader)
    unless compileOk $ do
        log <- filter isPrint <$> get (extShaderInfoLog shader)
        errorM "Kage" ("could not compile shader " ++ show shaderName ++ ": " ++ log)
    return compileOk

linkProgram' program = do
    linkExtProgram program
    checkProgramLinking program
    valid <- isValidProgram program
    unless valid (dumpProgram program)

checkProgramLinking :: ExtProgram -> IO Bool
checkProgramLinking program = do
    linkOk <- get (extLinkStatus program)
    unless linkOk $ do
        log <- filter isPrint <$> get (extProgramInfoLog program)
        errorM "Kage" ("could not link shader program " ++ show program ++ ": " ++ log)
    return linkOk

dumpProgram :: ExtProgram -> IO ()
dumpProgram program = do
    infoM "Kage" $ "Program " ++ show program
    shaders <- get (attachedExtShaders program)
    forM_ shaders $ \s -> infoM "Kage" $ "\t" ++ intercalate " / " [show s]
    {-
    attribs <- get (activeAttribs program)
    forM_ attribs $ \(i, varType, value) -> do
        location <- get (attribLocation program value)
        infoM "Kage" $ "\tAttribute: " ++ intercalate " / " [show i, show varType, value, show location]
    uniforms <- get (activeUniforms program)
    forM_ uniforms $ \(i, varType, value) -> do
        location <- get (uniformLocation program value)
        infoM "Kage" $ "\tUniforms: " ++ intercalate " / " [show i, show varType, value, show location]
    -}

isValidProgram :: ExtProgram -> IO Bool
isValidProgram program = do
    validateExtProgram program
    get (extValidateStatus program)

importExpr = RE.mkRegexWithOpts "^\\s*#include\\s+\"(.*)\"" True True

readFileContentWithImports fileName = do
    let readlines mp f = do
            debugM "Kage" ("Reading file " ++ f)
            content <- readFile (maybe f (`replaceFileName` f) mp)
            concat <$> forM (lines content) (\l -> case RE.matchRegex importExpr l of
                Just m -> readlines (Just f) (head m)
                Nothing -> return [l])
    ls <- readlines Nothing fileName
    return (Text.encodeUtf8 . Text.pack $ intercalate "\n" ls)

readFileContent :: String -> IO BS.ByteString
readFileContent fileName = Text.encodeUtf8 . Text.pack <$> readFile fileName
