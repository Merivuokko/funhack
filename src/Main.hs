-- | Main module for FunHack.

module Main (main) where

import Data.Text.IO qualified as TIO
import Effectful
import Effectful.Prim

import FunHack.DungeonGenerator

-- | Main entry point of funhack.
main :: IO ()
main = runEff $! runPrim
       $! makeNetHackLevel 40 20
       >>= showLevelMap
       >>= liftIO . TIO.putStr
