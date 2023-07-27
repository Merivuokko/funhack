-- | Main module for FunHack.

module Main (main) where

import Data.Text.IO qualified as TIO
import Effectful
import Effectful.Prim

import FunHack.DungeonGenerator
import FunHack.Geometry

-- | Main entry point of funhack.
main :: IO ()
main = runEff $! runPrim
       $! makeNetHackLevel (makeBox (Point 0 0 0) 40 20 1)
       >>= showLevelMap
       >>= liftIO . TIO.putStr
