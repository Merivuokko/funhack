-- | Simple NetHack-like dungeon leel generator

module FunHack.DungeonGenerator
    (
        -- * Level cells
        LevelCell (..),

        -- * Level map
        LevelMap,
        makeLevelMap
    ) where

import Data.Vector.Mutable as MV

import FunHack.Geometry

-- | LevelCell defines all cell types used by the level generator to fill
-- dungeon levels. These are not the same as World cells, because level
-- generator does not need to worry about many details whereas some of the
-- semantic information stored in LevelCells is not meaningful after the level
-- generation.
data LevelCell
    = Undefined -- ^ Unassigned space, may be used for rooms and corridors
    | RoomFloor -- ^ Floor of a room, may not be used for corridors.
    | RoomWall -- ^ Wall of a room. May not be used for corridors.
    | Doorway -- ^ A doorway to a room. May not be used by corridors.
    | Corridor -- ^ Floor of a corridor
    deriving stock (Eq, Show)

-- | LevelMap is a 2-dimensional vector of LevelCells
type LevelMap s = MV.MVector s (MV.MVector s LevelCell)

-- | Make a new empty level map with the provided dimensions.
makeLevelMap
    :: (MV.PrimMonad m)
    => Distance -- ^ Width of the level map
    -> Distance -- ^ Height of the level map
    -> m (LevelMap (MV.PrimState m))
makeLevelMap width height
    = MV.replicateM (fromIntegral height) (MV.replicate (fromIntegral width) Undefined)
