module Common.Random
    (   RandomState
    ,   runRandom
    ,   runRandomIO
    ,   getRandom
    ,   getRandomR
    ) where

import Control.Applicative()
import Control.Monad.State
import System.Random

----------------------------------------------------------------------------------------------------

type RandomState a = State StdGen a

runRandom :: Int -> RandomState a -> a
runRandom n r = evalState r $ mkStdGen n

runRandomIO :: RandomState a -> IO a
runRandomIO r = randomIO >>= (return . flip runRandom r)

getRandom :: Random a => RandomState a
getRandom = do
    (v, g) <- random <$> get
    put g
    return v

getRandomR :: Random a => (a, a) -> RandomState a
getRandomR (minValue, maxValue) = do
    (v, g) <- randomR (minValue, maxValue) <$> get
    put g
    return v
