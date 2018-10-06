module Lighthouse (Node (Node), Workload (Workload), assignWorkload) where

import qualified Data.Map as Map

data Node = Node { nodeId :: String
                 , resources :: Map.Map String Float
                 , workloads :: [Workload]
                 } deriving (Show, Eq)

data Workload = Workload { loadId :: String
                         , requirements :: Map.Map String Float
                         } deriving (Show, Eq)

assignWorkload :: [Node] -> Workload -> Maybe [Node]
assignWorkload nodes load = Just nodes
