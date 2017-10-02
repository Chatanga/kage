module Shader
    (   createProgramWithShaders'
    ,   createProgramWithShaders
    ) where

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Graphics.Rendering.OpenGL
import System.FilePath
import System.Log.Logger

import FunctionalGL
import Misc

------------------------------------------------------------------------------------------------------------------------

createProgramWithShaders' :: String -> String -> IO (Program, Dispose)
createProgramWithShaders' vertexShaderName fragmentShaderName = createProgramWithShaders
    [ (vertexShaderName, VertexShader)
    , (fragmentShaderName, FragmentShader)
    ]

createProgramWithShaders :: [(String, ShaderType)] -> IO (Program, Dispose)
createProgramWithShaders shaderSources = do
    let readFileContent fileName = Text.encodeUtf8 . Text.pack <$> readFile fileName

    program <- createProgram

    shaders <- forM shaderSources $ \(shaderName, shaderType) -> do
        infoM "Kage" ("Loading shader " ++ shaderName ++ " as " ++ show shaderType)
        shader <- createShader shaderType
        vs <- readFileContent ("shaders" </> shaderName)
        shaderSourceBS shader $= vs
        compileOk <- compileShader' shaderName shader
        when compileOk $ attachShader program shader
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

compileShader' shaderName shader = do
    compileShader shader
    checkShaderCompilation shaderName shader

checkShaderCompilation :: String -> Shader -> IO Bool
checkShaderCompilation shaderName shader = do
    compileOk <- get (compileStatus shader)
    unless compileOk $ do
        log <- filter isPrint <$> get (shaderInfoLog shader)
        errorM "Kage" ("could not compile shader " ++ show shaderName ++ ": " ++ log)
    return compileOk

linkProgram' program = do
    linkProgram program
    checkProgramLinking program
    valid <- isValidProgram program
    unless valid (dumpProgram program)

checkProgramLinking :: Program -> IO Bool
checkProgramLinking program = do
    linkOk <- get (linkStatus program)
    unless linkOk $ do
        log <- filter isPrint <$> get (programInfoLog program)
        errorM "Kage" ("could not link shader program " ++ show program ++ ": " ++ log)
    return linkOk

dumpProgram :: Program -> IO ()
dumpProgram program = do
    infoM "Kage" $ "Program " ++ show program
    shaders <- get (attachedShaders program)
    forM_ shaders $ \s -> infoM "Kage" $ "\t" ++ intercalate " / " [show s]
    attribs <- get (activeAttribs program)
    forM_ attribs $ \(i, varType, value) -> do
        location <- get (attribLocation program value)
        infoM "Kage" $ "\tAttribute: " ++ intercalate " / " [show i, show varType, value, show location]
    uniforms <- get (activeUniforms program)
    forM_ uniforms $ \(i, varType, value) -> do
        location <- get (uniformLocation program value)
        infoM "Kage" $ "\tUniforms: " ++ intercalate " / " [show i, show varType, value, show location]

isValidProgram :: Program -> IO Bool
isValidProgram program = do
    validateProgram program
    get (validateStatus program)

