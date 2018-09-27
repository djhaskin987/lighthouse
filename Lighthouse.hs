import qualified Data.Map as Map

data Node = Node {
    resources :: Map.Map String Float
} deriving (Show)

data Workload = Workload {
    requirements :: Map.Map String Float
}

assignWorkload :: Map.Map String Node -> [String] -> Map.Map String Node
assignWorkload nodes j
assignWorkload Map.empty [] = Map.empty
