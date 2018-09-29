module Lighthouse (lightUp) where
import qualified Data.Map as Map

data Node = Node {
    nodeId :: String
  , resources :: Map.Map String Float
} deriving (Show)

data Workload = Workload {
    loadId :: String
  , requirements :: Map.Map String Float
}

lightUp :: String -> String
lightUp s = "LIGHT " ++ s ++ " LIGHT"
-- assignWorkload :: Map.Map String Node -> [String] -> Map.Map String Node
-- assignWorkload nodes j
-- assignWorkload Map.empty [] = Map.empty
