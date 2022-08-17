-- | Simple NetHack-like dungeon level generator

module FunHack.DungeonGenerator
    (
        -- * Level cells
        LevelCell (..),

        -- * Level map
        LevelMap,
        makeLevelMap,
        showLevelMap,

        -- ** Read and update level maps
        readLevelMap,
        writeLevelMap,
        modifyLevelMap,

        -- * Dungeon generation
        makeNetHackLevel,

        -- ** Rooms
        Room,
        makeRoom,
        makeRooms
    ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
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
    :: (PrimMonad m)
    => Distance -- ^ Width of the level map
    -> Distance -- ^ Height of the level map
    -> m (LevelMap (PrimState m))
makeLevelMap width height
    = MV.replicateM (fromIntegral height) (MV.replicate (fromIntegral width) Undefined)

-- | Turn a LevelMap into a Text value for showing. This is mostly useful for debugging.
showLevelMap
    :: (PrimMonad m)
    => LevelMap (PrimState m)
    -> m T.Text
showLevelMap = MV.foldM' foldLine T.empty
  where
    foldLine :: (PrimMonad m) => T.Text -> MV.MVector (PrimState m) LevelCell -> m T.Text
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

-- | Read a cell at a given point from a leel map.
--
-- This function fails with an exception if the location is invalid.
readLevelMap
    :: (PrimMonad m)
    => Point3D -- ^ The cell location
    -> LevelMap (PrimState m) -- ^ The map to read from
    -> m LevelCell
readLevelMap point lmap
    = let x = fromIntegral point.x
          y = fromIntegral point.y
      in MV.read lmap y >>= flip MV.read x

-- | Write a new value to a level map at a given point.
--
-- This function fails with an exception, if hte location is invalid.
writeLevelMap
    :: (PrimMonad m)
    => Point3D -- ^ The cell location
    -> LevelCell -- ^ The new cell value
    -> LevelMap (PrimState m) -- ^ The map to read from
    -> m ()
writeLevelMap point value lmap
    = let x = fromIntegral point.x
          y = fromIntegral point.y
      in MV.read lmap y >>= \vec -> MV.write vec x value

-- | Modify a cell in a level map at a given point.
--
-- This function fails with an exception, if hte location is invalid.
modifyLevelMap
    :: (PrimMonad m)
    => Point3D -- ^ The cell location
    -> (LevelCell -> LevelCell) -- ^ A function called with the old value and which produces a new cell value
    -> LevelMap (PrimState m) -- ^ The map to read from
    -> m ()
modifyLevelMap point f lmap
    = let x = fromIntegral point.x
          y = fromIntegral point.y
      in do
    vec <- MV.read lmap y
    val <- MV.read vec x
    MV.write vec x $! f val

-- | Make a NetHack-style level map with the given dimensions.
makeNetHackLevel
    :: (PrimMonad m)
    => Distance -- ^ Width of the level map
    -> Distance -- ^ Height of the level map
    -> m (LevelMap (PrimState m))
makeNetHackLevel width height = do
    -- Create level map
    level <- makeLevelMap width height

    -- Create rooms
    let levelRect = makeRectangle (Point3D 0 0 0) width height
    rooms <- makeRooms level levelRect
    pure $! level

-- | A room descriptor contains all necessary information to place a room on a
-- map and to connect it to other rooms on the level.
data Room = Room {
    -- | Occupied area
    rectangle :: Rectangle,

    -- | Whether the room has been connected to other rooms
    connections :: Int,

    -- | List of door positions
    doorways :: [Point3D]
    } deriving stock (Eq, Show)

-- | Make a room and place it on a level map. This modifies the map in place
-- and returns a room description.
makeRoom
    :: (PrimMonad m)
    => LevelMap (PrimState m) -- ^ Map to put the room on
    -> Rectangle -- ^ The rectangle that the rooms should occupy
    -> m Room
makeRoom level rect = do
    pure $! Room rect 0 []

-- | Make rooms on a level map and return them as a list.
--
-- The first argument is the level map to be used for placing the rooms. The
-- map is modified in place. The room areas are filled with floor symbols and
-- wallified with the wall symbols.
--
-- The second argument specifies the rectangle within which the rooms are
-- created. If the rectangle is bigger than the allocated map, bad things will
-- happen.
makeRooms
    :: forall m. (PrimMonad m)
    => LevelMap (PrimState m) -- ^ Map of the level (this is odified in place)
    -> Rectangle -- ^ The bounding rectangle for the created rooms
    -> m [Room]
makeRooms level rect = do
    loop [] [rect]
  where
    -- | Recursively create rooms in one of the provided rectangles. Already
    -- created rooms are passed as the first argument. Finally all created
    -- rooms are returned.
    loop :: [Room] -> [Rectangle] -> m [Room]
    loop rooms [] = pure $! rooms
    loop rooms (r : rects) = do
        let roomRect = makeRectangle (Point3D r.origin.x r.origin.y r.origin.z) ((r.width `div` 2) - 1) ((r.height `div` 2) - 1)
        room <- makeRoom level roomRect

        let rectsAround = case splitRectangleAround r roomRect of
                              Just rs -> rs
                              Nothing -> error "Bad rectangle splitting"
            rects' = rectsAround <> rects
        pure $! (room : rooms)
