-- |
-- Module      : FunHack.Cell
-- Description : Game world cells
-- Copyright   : Copyright (C) 2022–2024 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Portability : GHC
--
-- Fundamental game world cell management
module FunHack.Cell (
    -- * Cells
    Cell (..),
    CellType (..),
    CellDescriptor (..),
) where

import Data.HashSet qualified as HS

-- | Cell represents a cubic portion of the game World. Its side length is fixed to 1 meter.
data Cell = Cell
    { -- | Dungeon feature
      feature :: CellType,
      -- | Descriptors of the cell
      descriptors :: HS.HashSet CellDescriptor
    }
    deriving stock (Eq, Show)

-- | CellType represents the type of a single world Cell.
data CellType
    = -- | Nothing, pure space, vacuum
      Void
    | -- | Breatheable air
      Air
    | -- | Stone with its hardness where 0=normal
      Stone Int
    deriving stock (Eq, Show)

data CellDescriptor
    = -- | Blocker from a point of view of the path generator
        RouteBlocker
    deriving stock (Eq, Show)
