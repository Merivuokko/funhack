-- | Path finding algorithms

module FunHack.PathFinding
    (
        -- * The A* algorithm
        aStarM
    ) where

import Control.Monad (foldM)
import Data.Bool (bool)
import Data.Bifunctor (second)
import Data.HashMap.Strict qualified as HMap
import Data.Hashable (Hashable)
import Data.PQueue.Prio.Min qualified as PQ

-- | A* algorith uses lots of state variables. This type makes the algorithm
-- more readable.
data AStarState a cost = AStarState {
    -- | The queue of discovered nodes that may need to be (re-)expanded.
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
    predecessors :: HMap.HashMap a a
    } deriving stock (Show)

-- | This function returns the shortest path between two nodes in a graph.
-- This is a general implementation which works by calling supplied functions
-- for querying all relevant information about the graph used.
aStarM
    :: forall a cost m. (Hashable a, Monad m, Ord cost, Num cost, Show cost, Show a)
    => (a -> m Bool) -- ^ Determine if the supplied node is the goal node.
    -> (a -> m cost) -- ^ A heuristic action that returns the approximate cost
                     -- of reaching the goal node from the node supplied. This
                     -- must be admissive (i.e. must not overestimate the
                     -- cost), or the shortest path may not be found.
    -> (a -> m [(a, cost)]) -- ^ An action that produces a list of all
                            -- adjacent nodes to the node supplied to it,
                            -- along with the cost of reaching that node.
    -> a -- ^ The starting node
    -> m (Maybe (cost, [a])) -- ^ Nothing, if no path was found. Else the cost
                           -- of the found path and the path as a list of
                           -- nodes.
aStarM isGoal heuristic adjacents startNode = do
    startHeur <- heuristic startNode
    aStarM' $! AStarState {
          queue = PQ.singleton (startHeur) (startNode, 0),
          travelCosts = HMap.singleton startNode 0,
          predecessors = HMap.empty
      }
  where
    -- The actual A* implementation
    aStarM' :: AStarState a cost -> m (Maybe (cost, [a]))
    aStarM' state
        = case PQ.minView state.queue of
              Nothing -> pure $! Nothing
              Just ((node, travelCost), rest) ->
                  let state' = state { queue = rest }
                  in searchFrom state' node travelCost

    -- Search path from a given node
    searchFrom :: AStarState a cost -> a -> cost -> m (Maybe (cost, [a]))
    searchFrom state node travelCost
        = isGoal node >>= bool (searchAdjacents >>= aStarM') (fmap Just $! resolvePath)
      where
        -- Search through all adjacent nodes the current node
        searchAdjacents :: m (AStarState a cost)
        searchAdjacents
            = adjacentNodesCosts
              >>= foldM (enqueue node) state

        -- A monadic action returning a list of adjacent nodes associated with their travel cost
        adjacentNodesCosts :: m [(a, cost)]
        adjacentNodesCosts
            = adjacents node
            >>= pure . (fmap $ second (+ travelCost))
            >>= pure . filter (\(n, c) ->
                                   ((<) <$> (Just c) <*> HMap.lookup n state.travelCosts) /= Just False)

        -- Resolve the shortest path once the goal has been found
        resolvePath :: m (cost, [a])
        resolvePath = pure $! (travelCost, constructPath (Just node) [])

        -- Do the actual work of constructing the path
        constructPath :: Maybe a -> [a] -> [a]
        constructPath Nothing prefix = prefix
        constructPath (Just n) prefix
            = constructPath (HMap.lookup n state.predecessors) (n : prefix)

    -- Add a node to the queue
    enqueue :: a -> AStarState a cost -> (a, cost) -> m (AStarState a cost)
    enqueue parent !state !(node, travel) = do
        heur <- heuristic node
        pure $! state {
              queue = PQ.insert (travel + heur) (node, travel) state.queue,
              travelCosts = HMap.insert node travel state.travelCosts,
              predecessors = HMap.insert node parent state.predecessors
          }
