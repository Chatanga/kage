module Lib (
    kickOut
) where

import Data.Time.Clock
import System.IO
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

import Graphics.Application
import Graphics.Font

kickOut = do
    hSetBuffering stdout NoBuffering

    h <- fileHandler "debug.log" DEBUG >>= \lh -> return $
        setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger "Kage" (addHandler h)
    updateGlobalLogger "Kage" (setLevel INFO)

    let line = replicate 80 '─'
    infoM "Kage" line

    -- runDefaultApplication "Playground"
    runAnotherApplication "Playground"

    infoM "Kage" line

generateFontAtlas = do
    t0 <- getCurrentTime
    -- "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
    createDistanceFieldAtlas "/usr/share/fonts/truetype/fonts-japanese-gothic.ttf"
    t1 <- getCurrentTime
    putStrLn $ "duration: " ++ show (diffUTCTime t1 t0)
