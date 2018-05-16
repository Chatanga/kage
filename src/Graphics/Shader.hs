module Graphics.Shader
    (   acquireProgramWithShaders'
    ,   acquireProgramWithShaders
    ,   createProgramWithShaders'
    ,   createProgramWithShaders
    ) where

import Control.Monad
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Char
import Data.List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Foreign (allocaArray, alloca, peek, plusPtr, withArray, peekArray, nullPtr, Ptr, castPtr)
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.GL as Raw
import System.FilePath
import System.Log.Logger
import qualified Text.Regex as RE

import Common.Debug
import Common.Misc

import Graphics.Ext.Program
import Graphics.Ext.Shader
import Graphics.FunctionalGL

----------------------------------------------------------------------------------------------------

acquireProgramWithShaders' :: String -> String -> ResourceIO (ExtProgram, ResourceIO ())
acquireProgramWithShaders' vertexShaderName fragmentShaderName = acquireProgramWithShaders
    [ (vertexShaderName, VertexShader)
    , (fragmentShaderName, FragmentShader)
    ]

acquireProgramWithShaders :: [(String, ShaderType)] -> ResourceIO (ExtProgram, ResourceIO ())
acquireProgramWithShaders shaderSources = do
    program <- liftIO createExtProgram

    shaders <- forM shaderSources $ \(shaderName, shaderType) ->
        acquireResourceWith shaderName $ do
            infoM "Kage" ("Loading shader " ++ shaderName ++ " as " ++ show shaderType)
            shader <- createExtShader shaderType
            vs <- readFileContentWithImports ("shaders" </> shaderName)
            extShaderSourceBS shader $= vs
            compileOk <- compileShader' shaderName shader
            -- when compileOk $ attachExtShader program shader
            return shader

    liftIO $ forM shaders (attachExtShader program)

    {- for vertex shaders
    attribLocation program "xxx" $= AttribLocation N
    (see AttribLocation in vertexAttribPointer and vertexAttribArray)
    -}

    {- for fragment shaders
    bindFragDataLocation program "xxx" $= N
    (see ColorAttachment in framebufferTexture2D)
    -}
    liftIO $ linkProgram' program

    let dispose = do
            liftIO $ deleteObjectName program
            mapM_ (releaseResource . fst) shaderSources

    return (program, dispose)

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

    {- for vertex shaders
    attribLocation program "xxx" $= AttribLocation N
    (see AttribLocation in vertexAttribPointer and vertexAttribArray)
    -}

    {- for fragment shaders
    bindFragDataLocation program "xxx" $= N
    (see ColorAttachment in framebufferTexture2D)
    -}
    linkProgram' program

    -- Delete the shaders as the program has them now.
    deleteObjectNames shaders

    return (program, deleteObjectName program)

-- Work in progress.
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
    compileOk <- GL.get (extCompileStatus shader)
    unless compileOk $ do
        log <- format <$> GL.get (extShaderInfoLog shader)
        errorM "Kage" ("could not compile shader " ++ show shaderName ++ ": " ++ log)
    return compileOk

linkProgram' program = do
    linkExtProgram program
    checkProgramLinking program
    valid <- isValidProgram program
    unless valid (dumpProgram program)

checkProgramLinking :: ExtProgram -> IO Bool
checkProgramLinking program = do
    linkOk <- GL.get (extLinkStatus program)
    unless linkOk $ do
        log <- format <$> GL.get (extProgramInfoLog program)
        errorM "Kage" ("could not link shader program " ++ show program ++ ": " ++ log)
    return linkOk

format :: String -> String
format text = intercalate "\n\t" (lines text)

dumpProgram :: ExtProgram -> IO ()
dumpProgram program = do
    infoM "Kage" $ "Program " ++ show program
    shaders <- GL.get (attachedExtShaders program)
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
    GL.get (extValidateStatus program)

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
