module Lib (
    kickOut
) where

import System.IO
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

import Data.Time.Clock

import Scene
import Application

import Font

import View (testProcessEvent)
kickOutOld = testProcessEvent

kickOut = do
    hSetBuffering stdout NoBuffering

    h <- fileHandler "debug.log" DEBUG >>= \lh -> return $
        setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger "Kage" (addHandler h)
    updateGlobalLogger "Kage" (setLevel INFO)
    infoM "Kage" (replicate 80 '-')

    run "Playground"

    {-
    t0 <- getCurrentTime
    -- createDistanceFieldLetter "/usr/share/fonts/truetype/fonts-japanese-gothic.ttf" 'K'
    -- createDistanceFieldAtlas "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
    createDistanceFieldAtlas "/usr/share/fonts/truetype/fonts-japanese-gothic.ttf"
    t1 <- getCurrentTime
    putStrLn $ "duration: " ++ show (diffUTCTime t1 t0)
    -}

    infoM "Kage" (replicate 80 '-')
