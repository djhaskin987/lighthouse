
module Lighthouse.TestUtilities (
  TestNode,
  begoneMaybe
  ) where

import Lighthouse
import           Data.Text        (Text, pack)

type TestNode = Node Text Text Text Int
begoneMaybe :: Maybe (Lighthouse.RoomBased Text Text Text Int)
            -> Lighthouse.RoomBased Text Text Text Int
begoneMaybe x = case x of
                  Nothing -> Lighthouse.emptyRoomBased
                    :: RoomBased Text Text Text Int
                  Just y -> y :: RoomBased Text Text Text Int
