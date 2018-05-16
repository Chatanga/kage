module Common.Misc
    ( for
    , divR
    , clamp
    , doNothing
    , doNothing1
    , mapMaybeM
    , adapt
    ) where

import Control.Monad
import Data.Maybe

----------------------------------------------------------------------------------------------------

for = flip map

divR :: (Real a, Real b, RealFloat c) => a -> b -> c
divR a b = realToFrac a / realToFrac b

clamp minValue maxValue = max minValue . min maxValue

doNothing :: IO ()
doNothing = return ()

doNothing1 :: a -> IO a
doNothing1 = return

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = catMaybes <$> mapM f xs

adapt :: (Integral a, Integral b) => (a -> a) -> (b -> b)
adapt f = fromIntegral . f . fromIntegral
