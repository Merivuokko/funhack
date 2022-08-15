-- | Simple NetHack-like dungeon leel generator

module FunHack.DungeonGenerator
    (
        -- * Level cells
        LevelCell (..),

        -- * Level map
        LevelMap,
        makeLevelMap,
        showLevelMap,

        -- * Dungeon generation
        makeNetHackLevel
    ) where

import Data.Text qualified as T
import Data.Vector.Mutable qualified as MV

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

-- | Turn a LevelMap into a Text value for showing. This is mostly useful for debugging.
showLevelMap
    :: (MV.PrimMonad m)
    => LevelMap (MV.PrimState m)
    -> m T.Text
showLevelMap = MV.foldM' foldLine T.empty
  where
    foldLine :: (MV.PrimMonad m) => T.Text -> MV.MVector (MV.PrimState m) LevelCell -> m T.Text
    foldLine prefix line = do
        textLine <- MV.foldl' cellsToText T.empty line
        pure $! prefix <> textLine <> "\n"

    cellsToText :: T.Text -> LevelCell -> T.Text
    cellsToText prefix cell = prefix <> (T.singleton $! cellChar $! cell)

    cellChar :: LevelCell -> Char
    cellChar cell = case cell of
        Undefined -> '_'
        RoomFloor -> '.'
        RoomWall -> '#'
        Doorway -> '+'
        Corridor -> ','

-- | Make a NetHack-style level map with the given dimensions.
makeNetHackLevel
    :: (MV.PrimMonad m)
    => Distance -- ^ Width of the level map
    -> Distance -- ^ Height of the level map
    -> m (LevelMap (MV.PrimState m))
makeNetHackLevel width height = do
    level <- makeLevelMap width height
    pure $! level
