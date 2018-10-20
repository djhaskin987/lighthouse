module Lighthouse (Node (Node), Workload (Workload), assignWorkload, place, nodeId, resources, workloads, loadId, requirements) where

import qualified Data.Map as Map
import qualified Control.Monad as Monad

data Node = Node { nodeId :: String
                 , resources :: Map.Map String Float
                 , workloads :: [Workload]
                 } deriving (Show, Eq)

data Workload = Workload { loadId :: String
                         , requirements :: Map.Map String Float
                         } deriving (Show, Eq)

-- |Replace the first element of the list for which the computation `(a ->
-- Maybe a)` was successful with its result, or return Nothing if no
-- computation worked for the whole list.
place :: (a -> Maybe a) -> [a] -> Maybe [a]
place f [] = Nothing
place f (x:xs) = case f x of
                 Nothing -> case place f xs of
                              Nothing -> Nothing
                              Just ys -> Just (x:ys)
                 Just y -> Just (y:xs)

attachWorkload :: Workload -> Node -> Maybe Node
attachWorkload load (Node id have loads)
  | Map.size used < Map.size need = Nothing
  | not isAllPositive = Nothing
  | otherwise =
    Just (Node
      id
      leftovers
      (load : loads))
  where
    -- Subtract what is required from what is available, and only show the
    -- difference for the keys that exist in both
    used = Map.intersectionWith
                  (-)
                  have
                  need
    need = requirements load
    -- Check to make sure all that there were in fact enough resources
    -- to meet the requirements' demands
    isAllPositive = Map.foldr
      (\v c -> v >= 0 && c)
      True
      used
    -- Update values in `have` with values in `used`
    leftovers = Map.unionWith
      (flip const)
      have
      used

assignWorkload :: [Node] -> Workload -> Maybe [Node]
assignWorkload nodes load =
  place (attachWorkload load) nodes

sortNodes :: [Node] -> [Node]
sortNodes nodes = nodes

sortWorkloads :: [Workload] -> [Workload]
sortWorkloads workloads = workloads

assignWorkloads :: [Node] -> [Workload] -> Maybe [Node]
assignWorkloads nodes loads = Monad.foldM assignWorkload sortedNodes sortedLoads
  where
    sortedNodes = sortNodes nodes
    sortedLoads = sortWorkloads loads

-- The above is a lot to bite off, so we'll work on going from a list of nodes
-- and their workloads to a list of workloads and their nodes using ix-set
-- another time.
