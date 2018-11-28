{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Lighthouse (
  Node (Node),
  ResourceManager (ResourceManager),
  Workload (Workload),
  NodeID,
  WorkloadID,
  assignWorkload,
  assignWorkloads,
  mgrNodes,
  mgrAssignments,
  fromListRR,
  loadId,
  nodeId,
  place,
  requirements,
  resources
                  ) where

import           Data.Aeson       hiding (json)
import           Data.Sequence((><))
import           Data.Text        (Text, pack)
import           GHC.Generics
import qualified Control.Monad as Monad
import qualified Data.Foldable as Fold
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Sequence

data Workload w r n =
  Workload { loadId :: w
           , requirements :: Map.Map r n
           } deriving (Show, Eq, Generic)


data Node i w r n = Node { nodeId :: i
                 , resources :: Map.Map r n
                 , assignedWorkloads :: Map.Map w (Workload w r n)
                 } deriving (Show, Eq, Generic)

class Distributor d where
  place :: ((Node i w r n) -> (Maybe (Node i w r n)))
                           -> d (Node i w r n)
                           -> Maybe (d (Node i w r n), (Node i w r n))

instance Distributor [] where
  -- |Replace the first element of the list for which the computation `(a ->
  -- Maybe a)` was successful with its result, or return Nothing if no
  -- computation worked for the whole list.
  place f [] = Nothing
  place f (x:xs) = case f x of
                   Nothing -> case place f xs of
                                Nothing -> Nothing
                                Just (ys, found) -> Just ((x:ys), found)
                   Just y -> Just ((y:xs), y)

findFirstGoodIdx  :: (a -> Maybe a) -> a -> (Int,Maybe a) -> (Int,Maybe a)
findFirstGoodIdx f a (c1,c2) = case (f a) of
                        Nothing -> (c1+1,c2)
                        Just y -> (0, Just y)

newtype RoundRobin a =
  RoundRobin { robinThings :: Sequence.Seq a } deriving (Show, Eq)

instance Distributor RoundRobin where
  place f (RoundRobin things) =
    case found of
      Nothing -> Nothing
      Just newThing -> Just $ (RoundRobin (right >< left), newThing)
    where
      left = Sequence.take (index + 1) things
      right = Sequence.drop (index + 1) things
      (index, found) = Fold.foldr (findFirstGoodIdx f) (0, Nothing) things

fromListRR :: (Ord i, Ord w, Ord r, Num n) => [(Node i w r n)] -> RoundRobin (Node i w r n)
fromListRR nodeList = RoundRobin $ Sequence.fromList nodeList

findFirstGood  :: (a -> Maybe a) -> a -> Maybe a -> Maybe a
findFirstGood f a c = case (f a) of
                        Nothing -> c
                        Just y -> Just y

score :: (Ord k, Num n) => Map.Map k n -> Map.Map k n -> Maybe n
score rubric parts
  | Map.size scored < Map.size rubric = Nothing
  | otherwise =
    Map.foldr (+) (fromInteger 0) scored
  where
    scored = Map.intersectionWith (*) rubric parts

data RoomBased i r n a =
  RoomBased { roomRubric :: Map.Map r n
            , roomScores :: Map.Map i n
            , roomThings :: Map.Map (n, i) a
            } deriving (Eq, Show)

fromListRB :: (Ord i, Ord w, Ord r, Num n)
           => Map.Map i n
           -> [(Node i w r n)]
           -> Maybe (RoomBased i r n (Node i w r n))
fromListRB rubric nodeList = RoomBased rubric nodeScores nodes
  where
    (nodeScores, nodes) =
      foldr (\v (c1,c2) ->
              let nodeScore = score rubric (resources v) in
                  ((Map.insert (nodeId v) nodeScore c1),
                   (Map.insert (nodeScore, (nodeId v)) v c2)))
            (Map.empty, Map.empty)
            nodeList

instance Distributor (RoomBased i r n) where
  place f (RoomBased ru sc th) = do
    found <- Fold.foldr (findFirstGood f) Nothing th
    newScore <- score ru (resources found)
    oldScore <- Map.lookup (nodeId found) sc
    return $ (RoomBased
                ru
                (Map.insert found newScore sc)
                (Map.insert (newScore, (nodeId found)) found
                  (Map.delete (oldScore,(nodeId found)) th)))
-- TODO continue parameterizing types by ordered id and numerical type
--
data ResourceManager d i w =
  ResourceManager { mgrNodes :: d
                  , mgrAssignments :: (Map.Map w i)
                  } deriving (Eq, Show)


canAttachWorkload :: (Workload w r n)
                  -> (Node i w r n)
                  -> Bool
canAttachWorkload load node =
  case (attachWorkload load node) of
    Nothing -> False
    Just _ -> True

attachWorkload :: (Workload w r n)
               -> (Node i w r n)
               -> Maybe (Node i w r n)
attachWorkload load (Node id have attached)
  | Map.size used < Map.size need = Nothing
  | not isAllPositive = Nothing
  | otherwise =
    Just (Node
      id
      leftovers
      (Map.insert (loadId load) load attached))
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

assignWorkload :: Distributor d
               => (ResourceManager (d (Node i w r n)) i w)
               -> (Workload w r n)
               -> Maybe (ResourceManager (d (Node i w r n)) i w)
assignWorkload (ResourceManager nodes assignments) load = do
  (newNodes, foundNode) <- place (attachWorkload load) nodes
  return $ ResourceManager
            newNodes $
            Map.insert
              (loadId load)
              (nodeId foundNode)
              assignments

sortNodes :: [(Node i w r n)] -> [(Node i w r n)]
sortNodes nodes = nodes

sortWorkloads :: [(Workload w r n)] -> [(Workload w r n)]
sortWorkloads workloads = workloads

assignWorkloads :: Distributor d
                => (ResourceManager (d (Node i w r n)) i w)
                -> [(Workload w r n)]
                -> Maybe (ResourceManager (d (Node i w r n)) i w)
assignWorkloads mgr loads =
  Monad.foldM assignWorkload mgr loads

-- The above is a lot to bite off, so we'll work on going from a list of nodes
-- and their workloads to a list of workloads and their nodes using ix-set
-- another time.

-- class Distributor t where
--   placeD :: (a -> Maybe a) -> t a -> Maybe (t a)


-- data RoomBased k num a = RoomBased
--   { identify :: a -> k
--   , score :: a -> num
--   , scores :: Map.Map k num
--   , roomThings :: Map.Map (k,num) a
--   }

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


