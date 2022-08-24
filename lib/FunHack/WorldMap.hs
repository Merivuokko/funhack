-- | A representation of topographic (both physical and meta-physical)
-- information about the structure (topology) of the game world.

module FunHack.WorldMap
    (
        -- * Generic world map handling
        WorldMap (..),
        makeWorldMap,
        refMap,
        setMap,
        modifyMap,
        modifyMapM
    ) where

import Control.Monad ((>=>))
import Data.Vector.Mutable qualified as MV
import Effectful
import Effectful.Prim

import FunHack.Geometry

-- | A type alias for mutable vectors used by this module
type Vector = MV.IOVector

-- | A world map is a mapping from points to world cells.
newtype WorldMap c = WorldMap (Vector (Vector (Vector c)))

-- | Make a new world map with bounds defined by a Box.
makeWorldMap :: Prim :> es => Box -> c -> Eff es (WorldMap c)
makeWorldMap box initial
    = WorldMap
      <$> (MV.replicateM (fromIntegral box.depth)
           $! MV.replicateM (fromIntegral box.height)
           $! MV.replicate (fromIntegral box.width) initial)

-- | A general utility function to execute an action, if another action
-- succeeds.
andThen :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
andThen v action = v >>= maybe (pure $! Nothing) action

-- | A utility function for referring to vector indices using Coords.
refVector :: Prim :> es => Coord -> Vector a -> Eff es (Maybe a)
refVector ix = flip MV.readMaybe $! fromIntegral $! ix

-- | A utility function for setting a vector element indexed by Coord.
setVector :: Prim :> es => Coord -> a -> Vector a -> Eff es (Maybe ())
setVector ix e v = do
    let ix' = fromIntegral $! ix
    if ix' < 0 || ix' > MV.length v
        then pure $! Nothing
        else fmap Just $! MV.unsafeWrite v ix' e

-- | Reference a point in the world map. Return Nothing if no such point exists.
refMap :: Prim :> es => Point -> WorldMap c -> Eff es (Maybe c)
refMap (Point { x, y, z }) (WorldMap wm)
    = (refVector z wm) `andThen` (refVector y) `andThen` (refVector x)

-- | Write a cell in a world map
setMap :: Prim :> es => Point -> c -> WorldMap c -> Eff es (Maybe ())
setMap (Point { x, y, z }) e (WorldMap wm)
    = (refVector z wm) `andThen` (refVector y) `andThen`  (setVector x e)

-- | Modify an element in a world map.
modifyMap :: Prim :> es => (c -> c) -> Point -> WorldMap c -> Eff es (Maybe ())
modifyMap f p wm
    = (refMap p wm) `andThen` \e -> setMap p (f e) wm

-- | Modify an element in a world map with a monadic action.
modifyMapM :: Prim :> es => (c -> Eff es c) -> Point -> WorldMap c -> Eff es (Maybe ())
modifyMapM action p wm
    = (refMap p wm) `andThen` (action >=> \e -> setMap p e wm)
