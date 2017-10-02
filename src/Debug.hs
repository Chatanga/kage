module Debug (
    trace,
    traceList
) where

import Data.List
import qualified Debug.Trace as Trace

trace :: Show a => String -> a -> a
trace t a = Trace.trace (t ++ " = " ++ show a) a
-- trace _ = id

traceList :: Show a => String -> [a] -> [a]
traceList t as = Trace.trace (t ++ " = [\n\t" ++ intercalate "\n\t" (map show as) ++ "\n]") as
--traceList _ = id
