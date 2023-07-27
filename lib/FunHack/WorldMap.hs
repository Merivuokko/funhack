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
        modifyMapM,
        renderMap
    ) where

import Control.Monad (foldM)
import Data.Text qualified as T
import Data.Vector.Mutable qualified as MV
import Effectful
import Effectful.Prim
import GHC.Stack (HasCallStack)

import FunHack.Geometry

-- | A type alias for mutable vectors used by this module
type Vector = MV.MVector PrimStateEff

-- | A world map is a mapping from points to world cells.
data WorldMap c = WorldMap {
    bounds :: Box,
    cells :: Vector c
    }

-- | Make a new world map with bounds defined by a Box.
makeWorldMap :: Prim :> es => Box -> c -> Eff es (WorldMap c)
makeWorldMap bounds initial = do
    cells <- MV.replicate (fromIntegral $! boxVolume bounds) initial
    pure $! WorldMap { bounds = bounds, cells = cells }

-- | Translate point to a worldmap cell index.
pointToIndex :: HasCallStack => Point -> WorldMap c -> Int
pointToIndex p wm
    = let !x = p.x - wm.bounds.origin.x
          !y = p.y - wm.bounds.origin.y
          !z = p.z - wm.bounds.origin.z
          !index = fromIntegral $! (wm.bounds.height * wm.bounds.width * z) + (wm.bounds.width * y) + x
          !len = MV.length wm.cells
      in if index >= 0 && index < len
         then index
         else error $! "Bad attempt to index a world map with region `"
              <> (show wm.bounds) <> "' at point `"
              <> (show p) <> "'"

-- | Reference a point in the world map. Return Nothing if no such point exists.
refMap :: (HasCallStack, Prim :> es) => Point -> WorldMap c -> Eff es c
refMap p wm
    = MV.unsafeRead wm.cells $! pointToIndex p wm

-- | Write a cell in a world map
setMap :: (HasCallStack, Prim :> es) => Point -> c -> WorldMap c -> Eff es ()
setMap p e wm
    = MV.unsafeWrite wm.cells (pointToIndex p wm) e

-- | Modify an element in a world map.
modifyMap :: Prim :> es => (c -> c) -> Point -> WorldMap c -> Eff es ()
modifyMap f p wm
    = let index = pointToIndex p wm
      in MV.unsafeModify wm.cells f index

-- | Modify an element in a world map with a monadic action.
modifyMapM :: Prim :> es => (c -> Eff es c) -> Point -> WorldMap c -> Eff es ()
modifyMapM action p wm
    = refMap p wm >>= action >>= \e -> setMap p e wm

-- | Render a WorldMap into text. 3D rendering is achieved through rendering
-- each horizontal plane of the map sequentially, starting from the topmost
-- one.
--
-- A function from the map's cell type to Char needs to be supplied for this
-- function. Unfortunately this does not allow for context-sensitive
-- rendering.
--
-- This function is mostly useful for debugging purposes.
renderMap :: forall c es. Prim :> es => (c -> Char) -> WorldMap c -> Eff es T.Text
renderMap f wm
    = let planeSize = fromIntegral $! wm.bounds.height * wm.bounds.width
          planeVecs = [ MV.slice (n * planeSize) planeSize wm.cells
                      | n <- [ 0 .. fromIntegral $! wm.bounds.depth - 1 ] ]
      in foldM renderPlane T.empty planeVecs
  where
    renderPlane :: T.Text -> Vector c -> Eff es T.Text
    renderPlane !acc !vec
        = let rowSize = fromIntegral $! wm.bounds.width
              rowVecs = [ MV.slice (n * rowSize) rowSize vec
                        | n <- [ 0 .. fromIntegral $! wm.bounds.height - 1 ] ]
          in foldM renderRow T.empty rowVecs
             >>= \cnt -> pure $! acc <> "*******\n" <> cnt <> "\n"

    renderRow :: T.Text -> Vector c -> Eff es T.Text
    renderRow !acc !vec = do
        row <- MV.foldl' (\a x -> a <> (T.singleton . f) x) T.empty vec
        pure $! acc <> row <> "\n"
