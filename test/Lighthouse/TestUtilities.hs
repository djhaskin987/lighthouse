
module Lighthouse.TestUtilities (
  TestNode,
  TestWorkload,
  begoneMaybe,
  defaultNode,
  defaultWorkload
  ) where

import Lighthouse
import           Data.Text        (Text, pack)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type TestNode = Node Text Text Text Int Text
type TestWorkload = Workload Text Text Int Text

defaultWorkload = Workload
  ""
  (Map.empty :: Map.Map Text Int)
  (Set.empty :: Set.Set Text)
  (Set.empty :: Set.Set Text)

defaultNode = Node
  ""
  (Map.empty :: Map.Map Text Int)
  (Map.empty :: Map.Map Text TestWorkload)

begoneMaybe :: Maybe (Lighthouse.RoomBased Text Text Text Int Text)
            -> Lighthouse.RoomBased Text Text Text Int Text
begoneMaybe x = case x of
                  Nothing -> Lighthouse.emptyRoomBased
                    :: RoomBased Text Text Text Int Text
                  Just y -> y :: RoomBased Text Text Text Int Text
