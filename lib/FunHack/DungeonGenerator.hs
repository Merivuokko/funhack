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

import Control.Monad (filterM, forM_, (<=<))
import Control.Monad.Primitive (RealWorld)
import Data.Bool (bool)
import Data.Text qualified as T
import Data.Vector.Mutable qualified as MV
import Effectful
import Effectful.Prim

import FunHack.Geometry
import FunHack.PathFinding

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

-- | LevelMap is a 2-dimensional vector of LevelCell coupled with additional datas
data LevelMap = LevelMap {
    -- | Cells of the level
    cells :: MV.MVector RealWorld (MV.MVector RealWorld LevelCell),

    -- | The bounding box of the level
    bounds :: Box
    }

-- | Make a new empty level map with the provided dimensions.
makeLevelMap
    :: Prim :> es
    => Distance -- ^ Width of the level map
    -> Distance -- ^ Height of the level map
    -> Eff es LevelMap
makeLevelMap width height = do
    cells <- MV.replicateM (fromIntegral height) (MV.replicate (fromIntegral width) Undefined)
    pure $! LevelMap {
        cells = cells,
        bounds = makeBox (Point 0 0 0) width height 1
    }

-- | Turn a LevelMap into a Text value for showing. This is mostly useful for debugging.
showLevelMap
    :: forall es. Prim :> es
    => LevelMap
    -> Eff es T.Text
showLevelMap = MV.foldM' foldLine T.empty . (.cells)
  where
    foldLine :: T.Text -> MV.MVector RealWorld LevelCell -> Eff es T.Text
    foldLine prefix line = do
        textLine <- MV.foldl' cellsToText T.empty line
        pure $! prefix <> textLine <> "\n"

    cellsToText :: T.Text -> LevelCell -> T.Text
    cellsToText prefix cell = prefix <> (T.singleton $! cellChar $! cell)

    cellChar :: LevelCell -> Char
    cellChar cell = case cell of
        Undefined -> ' '
        RoomFloor -> '.'
        RoomWall -> '#'
        Doorway -> '+'
        Corridor -> ','

-- | Read a cell at a given point from a level map.
--
-- This function fails with an exception if the location is invalid.
readLevelMap
    :: Prim :> es
    => Point -- ^ The cell location
    -> LevelMap -- ^ The map to read from
    -> Eff es LevelCell
readLevelMap point level
    = let x = fromIntegral point.x
          y = fromIntegral point.y
      in MV.read level.cells y >>= flip MV.read x

-- | Write a new value to a level map at a given point.
--
-- This function fails with an exception, if hte location is invalid.
writeLevelMap
    :: Prim :> es
    => Point -- ^ The cell location
    -> LevelCell -- ^ The new cell value
    -> LevelMap -- ^ The map to read from
    -> Eff es ()
writeLevelMap point value level
    = let x = fromIntegral point.x
          y = fromIntegral point.y
      in MV.read level.cells y >>= \vec -> MV.write vec x value

-- | Modify a cell in a level map at a given point.
--
-- This function fails with an exception, if hte location is invalid.
modifyLevelMap
    :: Prim :> es
    => Point -- ^ The cell location
    -> (LevelCell -> LevelCell) -- ^ A function called with the old value and which produces a new cell value
    -> LevelMap -- ^ The map to read from
    -> Eff es ()
modifyLevelMap point f level
    = let x = fromIntegral point.x
          y = fromIntegral point.y
      in do
    vec <- MV.read level.cells y
    val <- MV.read vec x
    MV.write vec x $! f val

-- | Make a NetHack-style level map with the given dimensions.
makeNetHackLevel
    :: Prim :> es
    => Distance -- ^ Width of the level map
    -> Distance -- ^ Height of the level map
    -> Eff es LevelMap
makeNetHackLevel width height = do
    -- Create level map
    level <- makeLevelMap width height

    -- Create rooms
    _rooms <- makeRooms level
    _ <- drawCorridor (Point 2 0 0) (Point 14 13 0) level
    _ <- drawCorridor (Point 2 0 0) (Point 22 16 0) level
    pure $! level

-- | A room descriptor contains all necessary information to place a room on a
-- map and to connect it to other rooms on the level.
data Room = Room {
    -- | Occupied area
    bounds :: Box,

    -- | Whether the room has been connected to other rooms
    connections :: Int,

    -- | List of door positions
    doorways :: [Point]
    } deriving stock (Eq, Show)

-- | Make a room and place it on a level map. This modifies the map in place
-- and returns a room description.
makeRoom
    :: Prim :> es
    => LevelMap -- ^ Map to put the room on
    -> Box -- ^ The box that the room should occupy
    -> Eff es Room
makeRoom !level !box = do
    forM_ (pointsInBox box) \p -> do
        let cell = bool RoomWall RoomFloor $! (isInner p)
        writeLevelMap p cell level
    pure $! Room box 0 []
  where
    -- Determine if a point is inside the region and not on its edge.
    isInner :: Point -> Bool
    isInner (Point { x, y })
        = y > south box && y < north box
          && x > west box && x < east box

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
    :: forall es. Prim :> es
    => LevelMap -- ^ Map of the level (this is odified in place)
    -> Eff es [Room]
makeRooms !level = do
    let z = level.bounds.origin.z
    let rects = [ makeBox (Point 1 1 z) 5 5 1,
                  makeBox (Point 10 3 z) 10 6 1,
                  makeBox (Point 20 12 z) 4 4 1,
                  makeBox (Point 12 14 z) 7 5 1
                ]
    loop rects
  where
    -- Recursively create rooms in one of the provided rectangles.
    -- Descriptors for created rooms are returned
    loop :: [Box] -> Eff es [Room]
    loop [] = pure $! []
    loop (r : rects) = do
        !room <- makeRoom level r
        !rooms <- loop rects
        pure $! room : rooms

        -- let rectsAround = case splitRectangleAround r roomRect of
        --                       Just rs -> rs
        --                       Nothing -> error "Bad rectangle splitting"
        --     rects' = rectsAround <> rects
        -- pure $! (room : rooms)

-- | Draw a corridor from the first point to the second.
--
-- Return a list of Point values showing the chosen path, or Nothing, if no
-- path was found.
drawCorridor
    :: forall es. Prim :> es
    => Point -- ^ Starting point of the corridor
    -> Point -- ^ Target point of the corridor
    -> LevelMap -- ^ The map
    -> Eff es (Maybe [Point])
drawCorridor start goal level = do
    findPathForCorridor >>= \case
        Just path -> do
            forM_ path \p -> writeLevelMap p Corridor level
            pure $! Just $! path
        Nothing -> pure $! Nothing
  where
    findPathForCorridor :: Eff es (Maybe [Point])
    findPathForCorridor
        = aStarM isGoal heuristic neighbours start
          >>= (pure . fmap snd)

    heuristic :: Point -> Eff es Distance
    heuristic p = pure $! (abs $! p.x - goal.x)
                  + (abs $! p.y - goal.y)
                  + (abs $! p.z - goal.z)

    isGoal :: Point -> Eff es Bool
    isGoal p = pure $! p == goal

    neighbours :: Point -> Eff es [(Point, Distance)]
    neighbours
        = mapM (pure . (, 1))
          <=< filterM isCorridorPoint . pointsAroundAlignedHorizontal

    -- Determine if a point is suitable for placing a corridor onto
    isCorridorPoint :: Point -> Eff es Bool
    isCorridorPoint !point
        = if boxContainsPoint level.bounds point
          then readLevelMap point level >>= pure . flip elem [Undefined, Corridor]
          else pure False
