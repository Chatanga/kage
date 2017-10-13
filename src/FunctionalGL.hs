{-# LANGUAGE ExistentialQuantification #-}

module FunctionalGL
    ( Render
    , Dispose
    , Object3D(..)
    , Context3D(..)
    , setUniform
    , getAndSet
    , withState
    , withBinding
    , withBuffer
    , withTexture2D
    , usingOrderedTextures
    , usingTextures
    , TemporaryValue (..)
    , with'
    ) where

{-  Destructor
    , ResourceMap
    , newResourceMap
    , acquireResource
    , acquireResourceWith
    , registerResource
    , releaseResource
-}

import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import Graphics.Rendering.OpenGL

import Ext.Program
import Ext.Uniform

----------------------------------------------------------------------------------------------------

-- TODO Virer Size.
type Render = Size -> ExtProgram -> IO ()

type Dispose = IO ()

-- TODO Renommer en Renderable.
data Object3D = Object3D
    {   o3d_program :: !ExtProgram
    ,   o3d_vao :: !VertexArrayObject
    ,   o3d_render :: !Render
    ,   o3d_dispose :: !Dispose
    }

instance Show Object3D where
    show _ = "<Object3D>"

data Context3D = Context3D
    {   c3d_program :: ExtProgram
    ,   c3d_render :: Size -> ExtProgram -> [Object3D] -> IO ()
    ,   c3d_dispose :: Dispose
    }

instance Show Context3D where
    show _ = "<Context3D>"

----------------------------------------------------------------------------------------------------

type Destructor = ResourceMap -> IO ResourceMap

type ResourceMap = Map.Map String (Int, (Resource, IO ()))

data Resource
    = ResourceTO TextureObject
    | ResourceBO BufferObject

class AsResource a where
    toResource :: a -> Resource
    fromResource :: Resource -> Maybe a
    deleteResource :: a -> IO ()

instance AsResource TextureObject where
    toResource = ResourceTO
    fromResource (ResourceTO to) = Just to
    fromResource _ = Nothing
    deleteResource = deleteObjectName

instance AsResource BufferObject where
    toResource = ResourceBO
    fromResource (ResourceBO to) = Just to
    fromResource _ = Nothing
    deleteResource = deleteObjectName

newResourceMap = Map.empty

acquireResource :: (GeneratableObjectName a, AsResource a) => ResourceMap -> String -> IO (ResourceMap, a)
acquireResource repository key = acquireResourceWith repository key (\_ -> return ())

acquireResourceWith :: (GeneratableObjectName a, AsResource a) => ResourceMap -> String -> (a -> IO ()) -> IO (ResourceMap, a)
acquireResourceWith repository key constructor = case Map.lookup key repository of
    Nothing -> do
        name <- genObjectName
        constructor name
        return (Map.insert key (1, (toResource name, deleteResource name)) repository, name)
    Just (counter, (resource, destructor)) ->
        case fromResource resource of
            Just name -> return (Map.insert key (counter + 1, (resource, destructor)) repository, name)
            Nothing -> error "Resource found with a different type"

registerResource :: AsResource a => ResourceMap -> String -> a -> ResourceMap
registerResource repository key name = case Map.lookup key repository of
    Nothing -> Map.insert key (1, (toResource name, deleteResource name)) repository
    Just (counter, (resource, destructor)) -> error ("Resource key already used: " ++ key)

releaseResource :: ResourceMap -> String -> IO ResourceMap
releaseResource repository key = case Map.lookup key repository of
    Nothing -> return repository
    Just (counter, (resource, destructor)) ->
        if counter > 1
            then return (Map.insert key (counter - 1, (resource, destructor)) repository)
            else do
                destructor
                return (Map.delete key repository)

----------------------------------------------------------------------------------------------------

setUniform :: Uniform a => ExtProgram -> String -> a -> IO ()
setUniform program variableName value = do
    loc <- get (extUniformLocation program variableName)
    uniform loc $= value

getAndSet :: StateVar a -> a -> IO a
getAndSet stateVar newValue = do
    oldValue <- get stateVar
    stateVar $= newValue
    return oldValue

withState :: StateVar a -> a -> IO b -> IO b
withState state value action = do
    oldValue <- get state
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

----------------------------------------------------------------------------------------------------

with :: (HasSetter g a, HasGetter g a) => g -> a -> IO t -> IO t
with var val action = do
    old <- get var
    var $= val
    ret <- action
    var $= old
    return ret

-- Better:

data TemporaryValue = forall a g. (HasSetter g a, HasGetter g a) => g := a

with' :: [TemporaryValue] -> IO t -> IO t
with' tvs act = do
    olds <- mapM (\(a := b) -> get a >>= \old -> return (a := old)) tvs
    mapM_ (\(a := b) -> a $= b) tvs
    ret <- act
    mapM_ (\(a := b) -> a $= b) olds
    return ret
