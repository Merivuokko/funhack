-- |
-- Module      : FunHack.PathFinding
-- Description : Path finding routines
-- Copyright   : Copyright (C) 2022–2024 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Portability : GHC
--
-- Path finding algorithms
module FunHack.PathFinding (
    -- * A* state
    AStarState,

    -- * The A* algorithm
    aStarM,
    makeAStarState,
    runAStarM,
) where

import Control.Monad (foldM)
import Data.Bifunctor (second)
import Data.HashMap.Strict qualified as HMap
import Data.Hashable (Hashable)
import Data.PQueue.Prio.Min qualified as PQ

-- | A* algorith uses lots of state variables. This type makes the algorithm
-- more readable.
data AStarState m a cost = AStarState
    { -- | The queue of discovered nodes that may need to be (re-)expanded.
      --
      -- The key of this mapping is the estimated cost of travelling to the
      -- target node through the node n (referred to by this key). This is often
      -- referred to as the fScore of a node.
      --
      -- The value of the mapping is a tuple with the node n and the lowest cost
      -- found so far to reach this n (the so called gScore).
      queue :: PQ.MinPQueue cost (a, cost),
      -- | A mapping from node n to the best travel cost to reach n seen so far
      travelCosts :: HMap.HashMap a cost,
      -- | A map from a node to its preceding node (along the shortest path found so far)
      predecessors :: HMap.HashMap a a,
      -- | Goal detection function
      isGoal :: a -> m Bool,
      -- | A heuristic action
      heuristic :: a -> m cost,
      -- | An action that produces a list of adjacent nodes
      adjacents :: a -> m [(a, cost)]
    }

-- | Determine the shortest path between two nodes in a graph. This is a
-- general implementation which works by calling supplied functions for
-- querying all relevant information about the graph used.
-- The result is a list which contains all possible paths with the shortest path first. The list elements are computed lazily.
aStarM
    :: forall a cost m
     . (Hashable a, Monad m, Ord cost, Num cost)
    => (a -> m Bool)
    -- ^ Determine if the supplied node is the goal node.
    -> (a -> m cost)
    -- ^ A heuristic action that returns the approximate cost
    -- of reaching the goal node from the node supplied. This
    -- must be admissive (i.e. must not overestimate the
    -- cost), or the shortest path may not be found.
    -> (a -> m [(a, cost)])
    -- ^ An action that produces a list of all
    -- adjacent nodes to the node supplied to it,
    -- along with the cost of reaching that node.
    -> a
    -- ^ The starting node
    -> m (Maybe (cost, [a]))
    -- ^ Nothing, if no path was found. Else the cost
    -- of the found path and the path as a list of
    -- nodes.
aStarM isGoal heuristic adjacents startNode = do
    state <- makeAStarState isGoal heuristic adjacents startNode
    result <- runAStarM state
    pure $! case result of
        Nothing -> Nothing
        Just (cost, path, _) -> Just (cost, path)

makeAStarState
    :: forall a cost m
     . (Hashable a, Monad m, Num cost)
    => (a -> m Bool)
    -- ^ Determine if the supplied node is the goal node.
    -> (a -> m cost)
    -- ^ A heuristic action that returns the approximate cost
    -- of reaching the goal node from the node supplied. This
    -- must be admissive (i.e. must not overestimate the
    -- cost), or the shortest path may not be found.
    -> (a -> m [(a, cost)])
    -- ^ An action that produces a list of all
    -- adjacent nodes to the node supplied to it,
    -- along with the cost of reaching that node.
    -> a
    -- ^ The starting node
    -> m (AStarState m a cost)
makeAStarState isGoal heuristic adjacents startNode = do
    startHeur <- heuristic startNode
    pure $!
        AStarState
            { queue = PQ.singleton (startHeur) (startNode, 0),
              travelCosts = HMap.singleton startNode 0,
              predecessors = HMap.empty,
              isGoal = isGoal,
              heuristic = heuristic,
              adjacents = adjacents
            }

-- | The actual A* implementation
runAStarM
    :: forall a cost m
     . (Hashable a, Monad m, Ord cost, Num cost)
    => AStarState m a cost
    -> m (Maybe (cost, [a], AStarState m a cost))
runAStarM origState =
    case PQ.minView origState.queue of
        Nothing -> pure Nothing
        Just ((node, travelCost), rest) ->
            let state' = origState {queue = rest}
            in  searchFrom state' node travelCost
  where
    -- Search path from a given node
    searchFrom :: AStarState m a cost -> a -> cost -> m (Maybe (cost, [a], AStarState m a cost))
    searchFrom state node travelCost = do
        state.isGoal node >>= \case
            True -> do
                (cost, path) <- resolvePath
                pure $! Just $! (cost, path, state)
            False ->
                searchAdjacents >>= runAStarM
      where
        -- Search through all adjacent nodes the current node
        searchAdjacents :: m (AStarState m a cost)
        searchAdjacents =
            adjacentNodesCosts
                >>= foldM (enqueue node) state

        -- A monadic action returning a list of adjacent nodes associated with their travel cost
        adjacentNodesCosts :: m [(a, cost)]
        adjacentNodesCosts =
            state.adjacents node
                >>= pure . (fmap $ second (+ travelCost))
                >>= pure
                    . filter
                        ( \(n, c) ->
                            ((<) <$> (Just c) <*> HMap.lookup n state.travelCosts) /= Just False
                        )

        -- Resolve the path to a goal node
        resolvePath :: m (cost, [a])
        resolvePath = pure $! (travelCost, constructPath (Just node) [])

        -- Do the actual work of constructing the path
        constructPath :: Maybe a -> [a] -> [a]
        constructPath Nothing prefix = prefix
        constructPath (Just n) prefix =
            constructPath (HMap.lookup n state.predecessors) (n : prefix)

    -- Add a node to the queue
    enqueue :: a -> AStarState m a cost -> (a, cost) -> m (AStarState m a cost)
    enqueue parent !state !(node, travel) = do
        heur <- state.heuristic node
        pure $!
            state
                { queue = PQ.insert (travel + heur) (node, travel) state.queue,
                  travelCosts = HMap.insert node travel state.travelCosts,
                  predecessors = HMap.insert node parent state.predecessors
                }
