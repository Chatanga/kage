{-# LANGUAGE ExistentialQuantification #-}

module Graphics.FunctionalGL
    ( ResourceIO
    , ResourceMap
    , newResourceMap
    , acquireResourceWith
    , acquireResourceWith'
    , releaseResource
    , releaseUnreclaimedResources
    , Stage(..)
    , ordinal
    , Render
    , Dispose
    , skyBoxZOrder
    , terrainZOrder
    , defaultZOrder
    , translucentZOrder
    , Renderable(..)
    , setUniform
    , getAndSet
    , Graphics.FunctionalGL.withState
    , withBinding
    , withBuffer
    , withTexture2D
    , usingOrderedTextures
    , usingTextures
    , withDrawFramebuffer
    , withDrawFramebuffer'
    , TemporaryValue (..)
    , with'
    ) where

import Control.Monad
import Control.Monad.State as S
import qualified Data.Map as Map
import Data.Maybe
import Graphics.Rendering.OpenGL as GL
import System.Log.Logger

import Graphics.Ext.Program
import Graphics.Ext.Shader
import Graphics.Ext.Uniform

----------------------------------------------------------------------------------------------------

type ResourceIO a = StateT ResourceMap IO a

type ResourceMap = Map.Map String (Int, (Resource, Destructor))

newtype Destructor = Destructor
    {   destructorFunction :: ResourceIO ()
    }

data Resource
    = ResourceTO TextureObject
    | ResourceBO BufferObject
    | ResourceS Shader
    | ResourceES ExtShader
    | ResourceR Renderable

class AsResource a where
    toResource :: a -> Resource
    fromResource :: Resource -> Maybe a
    deleteResource :: a -> Destructor

instance AsResource TextureObject where
    toResource = ResourceTO
    fromResource (ResourceTO to) = Just to
    fromResource _ = Nothing
    deleteResource = Destructor . liftIO . deleteObjectName

instance AsResource BufferObject where
    toResource = ResourceBO
    fromResource (ResourceBO bo) = Just bo
    fromResource _ = Nothing
    deleteResource = Destructor . liftIO . deleteObjectName

instance AsResource Shader where
    toResource = ResourceS
    fromResource (ResourceS s) = Just s
    fromResource _ = Nothing
    deleteResource = Destructor . liftIO . deleteObjectName

instance AsResource ExtShader where
    toResource = ResourceES
    fromResource (ResourceES es) = Just es
    fromResource _ = Nothing
    deleteResource = Destructor . liftIO . deleteObjectName

instance AsResource Renderable where
    toResource = ResourceR
    fromResource (ResourceR r) = Just r
    fromResource _ = Nothing
    deleteResource = Destructor . renderableDispose

newResourceMap = Map.empty

acquireResourceWith :: AsResource a => String -> IO a -> ResourceIO a
acquireResourceWith key constructor = acquireResourceWith' key (liftIO constructor)

acquireResourceWith' :: AsResource a => String -> ResourceIO a -> ResourceIO a
acquireResourceWith' key constructor = do
    resources <- S.get
    case Map.lookup key resources of
        Nothing -> do
            r <- constructor
            liftIO $ debugM "Kage" $ "Creating resource " ++ show key
            resources <- S.get
            S.put $ Map.insert key (1, (toResource r, deleteResource r)) resources
            return r
        Just (counter, (resource, destructor)) ->
            case fromResource resource of
                Just r -> do
                    S.put $ Map.insert key (counter + 1, (resource, destructor)) resources
                    return r
                Nothing -> error "Resource found with a different type"

releaseResource :: String -> ResourceIO ()
releaseResource key = do
    resources <- S.get
    case Map.lookup key resources of
        Nothing -> error $ "Unknown resource " ++ show key
        Just (counter, (resource, destructor)) ->
            if counter > 1
                then S.put $ Map.insert key (counter - 1, (resource, destructor)) resources
                else do
                    liftIO $ debugM "Kage" $ "Releasing resource " ++ show key
                    destructorFunction destructor
                    S.put $ Map.delete key resources

-- TODO Some resources could be release many times now that Renderables are resources too.
releaseUnreclaimedResources :: ResourceIO ()
releaseUnreclaimedResources = do
    resources <- S.get :: ResourceIO ResourceMap
    forM_ (Map.toList resources) $ \(key, (counter, (resource, destructor))) -> do
        liftIO $ debugM "Kage" $ "Releasing unreclaimed resource " ++ show key
        destructorFunction destructor
    S.put Map.empty

----------------------------------------------------------------------------------------------------

data Stage
    = ShadowMappingStage
    | DirectShadingStage
    | DeferredShadingStage
    deriving Eq

ordinal :: Stage -> GLint
ordinal stage = case stage of
    ShadowMappingStage -> 1
    DirectShadingStage -> 2
    DeferredShadingStage -> 3

type Render = ExtProgram -> IO ()

type Dispose = IO ()

skyBoxZOrder = 10 :: Int
terrainZOrder = 20 :: Int
defaultZOrder = 30 :: Int
translucentZOrder = 40 :: Int

data Renderable = Renderable
    {   renderableProgram :: [(Stage, ExtProgram)] -- ^ Exposed to bind uniforms.
    ,   renderableVao :: VertexArrayObject -- ^ Exposed, but shouldn't ; only used to display normals.
    ,   renderableRender :: Render
    ,   renderableDispose :: ResourceIO ()
    ,   renderableZOrder :: Int
    }

instance Show Renderable where
    show _ = "<Renderable>"

----------------------------------------------------------------------------------------------------

setUniform :: Uniform a => ExtProgram -> String -> a -> IO ()
setUniform program variableName value = do
    loc <- GL.get (extUniformLocation program variableName)
    uniform loc $= value

getAndSet :: StateVar a -> a -> IO a
getAndSet stateVar newValue = do
    oldValue <- GL.get stateVar
    stateVar $= newValue
    return oldValue

withState :: StateVar a -> a -> IO b -> IO b
withState state value action = do
    oldValue <- GL.get state
    state $= value
    r <- action
    state $= oldValue
    return r

withBinding :: StateVar (Maybe a) -> a -> IO b -> IO b
withBinding binding value action = do
    binding $= Just value
    r <- action
    binding $= Nothing
    return r

withBuffer :: BufferTarget -> BufferObject -> IO b -> IO b
withBuffer target buffer action = do
    bindBuffer target $= Just buffer
    r <- action
    bindBuffer target $= Nothing
    return r

withTexture2D :: TextureObject -> IO b -> IO b
withTexture2D texture action = do
    textureBinding Texture2D $= Just texture
    r <- action
    textureBinding Texture2D $= Nothing
    return r

usingOrderedTextures :: ExtProgram -> [TextureObject] -> IO () -> IO ()
usingOrderedTextures program textures = usingTextures program (zip [0..] textures)

usingTextures :: ExtProgram -> [(GLuint, TextureObject)] -> IO () -> IO ()
usingTextures program indexedTextures action = do
    forM_ indexedTextures $ \(i, t) -> do
        let uniformName = if length indexedTextures == 1
            then "sampler"
            else "samplers[" ++ show i ++ "]"
        setUniform program uniformName (TextureUnit i)
        activeTexture $= TextureUnit i
        textureBinding Texture2D $= Just t
    action
    forM_ indexedTextures $ \(i, t) -> do
        activeTexture $= TextureUnit i
        textureBinding Texture2D $= Nothing

withDrawFramebuffer :: FramebufferObject -> IO () -> IO ()
withDrawFramebuffer fbo action = do
    bindFramebuffer DrawFramebuffer $= fbo
    action
    -- TODO restore previous instead?
    bindFramebuffer DrawFramebuffer $= defaultFramebufferObject

withDrawFramebuffer' :: GL.Size -> FramebufferObject -> IO () -> IO ()
withDrawFramebuffer' size fbo action = do
    backupViewport <- GL.get viewport
    viewport $= (Position 0 0, size)
    withDrawFramebuffer fbo action
    viewport $= backupViewport

----------------------------------------------------------------------------------------------------

with :: (HasSetter g a, HasGetter g a) => g -> a -> IO t -> IO t
with var val action = do
    old <- GL.get var
    var $= val
    ret <- action
    var $= old
    return ret

-- Better:

data TemporaryValue = forall a g. (HasSetter g a, HasGetter g a) => g := a

with' :: [TemporaryValue] -> IO t -> IO t
with' tvs act = do
    olds <- mapM (\(a := b) -> GL.get a >>= \old -> return (a := old)) tvs
    mapM_ (\(a := b) -> a $= b) tvs
    ret <- act
    mapM_ (\(a := b) -> a $= b) olds
    return ret
