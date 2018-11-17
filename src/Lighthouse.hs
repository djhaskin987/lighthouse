{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Lighthouse (Node (Node), Workload (Workload), assignWorkloads, assignWorkload, place, nodeId, resources, workloads, loadId, requirements) where

import           Data.Aeson       hiding (json)
import           Data.Sequence((><))
import           Data.Text        (Text, pack)
import           GHC.Generics
import qualified Control.Monad as Monad
import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence

data Workload = Workload { loadId :: Text
                         , requirements :: Map.Map Text Float
                         } deriving (Show, Eq, Generic)

data Node = Node { nodeId :: Text
                 , resources :: Map.Map Text Float
                 , workloads :: [Workload]
                 } deriving (Show, Eq, Generic)

instance ToJSON Workload

instance FromJSON Workload

instance FromJSON Node

instance ToJSON Node

class Distributor t where
  placeD :: (a -> Maybe a) -> t a -> Maybe (t a)

findFirstGoodIdx  :: (a -> Maybe a) -> a -> (Int,Maybe a) -> (Int,Maybe a)
findFirstGoodIdx f a (c1,c2) = case (f a) of
                        Nothing -> (c1+1,c2)
                        Just y -> (0, Just y)

findFirstGood  :: (a -> Maybe a) -> a -> Maybe a -> Maybe a
findFirstGood f a c = case (f a) of
                        Nothing -> c
                        Just y -> Just y

newtype RoundRobin a = RoundRobin { robinThings :: Sequence.Seq a }

instance Distributor RoundRobin where
  placeD f d = case found of
                Nothing -> Nothing
                Just newThing -> Just (RoundRobin (
                  right ><
                  left ><
                  (Sequence.singleton newThing)))
    where
      left = Sequence.take index things
      right = Sequence.drop (index + 1) things
      (index, found) = Fold.foldr (findFirstGoodIdx f) (0,Nothing) things
      things = robinThings d

data RoomBased k num a = RoomBased
  { identify :: a -> k
  , score :: a -> num
  , scores :: Map.Map k num
  , roomThings :: Map.Map (k,num) a
  }

-- updateRoomBased :: (RoomBased k num a) -> a -> (RoomBased k num a)
-- updateRoomBased r newThing = RoomBased {
--   identify = ident,
--   score = sc,
--   scores = newScore,
--   roomThings = (remove oldScores  -- TODO
--
--   where
--     ident = identify r
--     sc = score r
--     newId = ident newThing
--     newScore = sc newThing
--     oldScores = scores r


-- instance Distributor (RoomBased k num a) where
--   placeD f d = case found of
--                 Nothing -> Nothing
--                 Just newThing -> Just (updateRoomBased newThing)
--     where
--       found = Map.foldr (findFirstGood f) Nothing roomThings d


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
