-- | Main module for FunHack.

module Main (main) where

import Data.Text.IO qualified as TIO

import FunHack.DungeonGenerator

-- | Main entry point of funhack.
main :: IO ()
main = makeNetHackLevel 40 20 >>= showLevelMap >>= TIO.putStr
