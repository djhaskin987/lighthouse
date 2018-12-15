{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lighthouse (
  Node (Node),
  ResourceManager (ResourceManager),
  Workload (Workload),
  RoomBased,
  assignWorkload,
  assignWorkloads,
  makeSimpleWorkload,
  mgrNodes,
  mgrAssignments,
  emptyRoomBased,
  fromListPR,
  fromListRR,
  fromListRB,
  loadId,
  nodeId,
  place,
  requirements,
  resources,
  sortWorkloads
                  ) where

import           Data.Aeson
import           Data.Sequence ((><), (<|), (|>), Seq(Empty, (:<|)))
import           Data.Sort (sortOn)
import           Data.Text        (Text, pack)
import           GHC.Generics
import qualified Control.Monad as Monad
import qualified Data.Foldable as Fold
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Sequence

data Workload w r n g =
  Workload { loadId :: w
           , requirements :: Map.Map r n
           , tolerations :: Set.Set r
           , aversionGroups :: Set.Set g
           } deriving (Show, Eq, Generic)

makeSimpleWorkload :: w -> Map.Map r n -> Workload w r n g
makeSimpleWorkload id reqs =
  Workload
    id
    reqs
    (Set.empty :: Set.Set r)
    (Set.empty :: Set.Set g)

data Node i w r n g = Node { nodeId :: i
                 , resources :: Map.Map r n
                 , assignedWorkloads :: Map.Map w (Workload w r n g)
                 } deriving (Show, Eq, Generic)

instance (FromJSON w,
          FromJSON r,
          FromJSONKey r,
          Ord r,
          FromJSON n,
          FromJSON g,
          FromJSONKey g,
          Ord g)
  => FromJSON (Workload w r n g) where
  parseJSON = withObject "Workload" $ \o -> do
    wid <- o .: "id"
    reqs <- o .: "requirements"
    tols <- o .:? "tolerations" .!= ([] :: [r])
    avrs <- o .:? "aversion_groups" .!= ([] :: [g])
    return $ Workload wid reqs (Set.fromList tols) (Set.fromList avrs)

instance (ToJSON w,
          ToJSON r,
          ToJSONKey r,
          Ord r,
          ToJSON n,
          ToJSON g)
  => ToJSON (Workload w r n g) where
  toJSON (Workload wid reqs tols avrs) =
    object [
      "id" .= wid,
      "requirements" .= reqs,
      "tolerations" .= (Set.toList tols),
      "aversion_groups" .= (Set.toList avrs)
    ]

instance (FromJSON i,
          FromJSON w,
          FromJSONKey w,
          Ord w,
          FromJSON r,
          FromJSONKey r,
          Ord r,
          FromJSON n,
          Num n,
          Ord n,
          FromJSON g,
          FromJSONKey g,
          Ord g)
  => FromJSON (Node i w r n g) where
  parseJSON = withObject "Node" $ \o -> do
    lid <- o .: "id"
    res <- o .: "resources"
    lds <- o .:? "workloads" .!= (Map.empty :: Map.Map w (Workload w r n g))
    return $ Node lid res lds

instance (ToJSON i,
          ToJSON w,
          ToJSONKey w,
          Ord w,
          ToJSON r,
          ToJSONKey r,
          Ord r,
          ToJSON n,
          ToJSONKey g,
          ToJSON g)
  => ToJSON (Node i w r n g) where
  toJSON (Node lid res lds) =
    object [
      "id" .= lid,
      "resources" .= res,
      "workloads" .= lds
    ]
class Distributor d where
  place :: (Ord i, Ord w, Ord r, Ord n, Num n, Ord g)
        => (Node i w r n g -> Maybe (Node i w r n g))
        -> d i w r n g
        -> Maybe (d i w r n g, Node i w r n g)

newtype Prioritized i w r n g =
  Prioritized { prioritizedThings :: Sequence.Seq (Node i w r n g)
              } deriving (Show, Eq)

instance Distributor Prioritized where
  -- |Replace the first element of the list for which the computation `(a ->
  -- Maybe a)` was successful with its result, or return Nothing if no
  -- computation worked for the whole list.
  place f (Prioritized Empty) = Nothing
  place f (Prioritized (x:<|xs)) =
    case f x of
      Nothing -> case place f (Prioritized xs) of
                   Nothing -> Nothing
                   Just (Prioritized ys, found) ->
                     Just (Prioritized (x <| ys), found)
      Just y -> Just (Prioritized (y <| xs), y)

fromListPR :: (Ord i, Ord w, Ord r, Num n, Ord g)
           => [Node i w r n g]
           -> Prioritized i w r n g
fromListPR nodeList = Prioritized $ Sequence.fromList nodeList

findFirstGoodIdx  :: (a -> Maybe a) -> a -> (Int,Maybe a) -> (Int,Maybe a)
findFirstGoodIdx f a (c1,c2) = case f a of
                        Nothing -> (c1+1,c2)
                        Just y -> (0, Just y)

newtype RoundRobin i w r n g =
  RoundRobin { robinThings :: Sequence.Seq (Node i w r n g)
             } deriving (Show, Eq)

instance Distributor RoundRobin where
  place f (RoundRobin things) =
    case found of
      Nothing -> Nothing
      Just newThing -> Just (RoundRobin (right ><
                                           Sequence.singleton newThing ><
                                           left), newThing)
    where
      left = Sequence.take index things
      right = Sequence.drop (index + 1) things
      (index, found) = Fold.foldr (findFirstGoodIdx f) (0, Nothing) things

fromListRR :: (Ord i, Ord w, Ord r, Ord n, Num n, Ord g)
           => [Node i w r n g]
           -> RoundRobin i w r n g
fromListRR nodeList = RoundRobin $ Sequence.fromList nodeList

findFirstGood  :: (a -> Maybe a) -> a -> Maybe a -> Maybe a
findFirstGood f a c = case f a of
                        Nothing -> c
                        Just y -> Just y

score :: (Ord k, Num n)
      => Map.Map k n
      -> Map.Map k n
      -> Maybe n
score rubric parts
  | Map.size scored < Map.size rubric = Nothing
  | otherwise =
    Just (Map.foldr (+) 0 scored)
  where
    scored = Map.intersectionWith (*) rubric parts

scorePartial :: (Ord k, Num n)
      => Map.Map k n
      -> Map.Map k n
      -> n
scorePartial rubric parts
  | Map.size scored < Map.size rubric = error "Insufficient rubric"
  | otherwise = Map.foldr (+) 0 scored
  where
    scored = Map.intersectionWith (*) rubric parts

data RoomBased i w r n g =
  RoomBased { roomRubric :: Map.Map r n
            , roomScores :: Map.Map i n
            , roomThings :: Map.Map (n, i) (Node i w r n g)
            } deriving (Eq, Show)

emptyRoomBased :: RoomBased i w r n g
emptyRoomBased = RoomBased Map.empty Map.empty Map.empty

fromListRB :: (Ord i, Ord w, Ord r, Ord n, Num n, Ord g)
           => Map.Map r n
           -> [Node i w r n g]
           -> Maybe (RoomBased i w r n g)
fromListRB rubric nodeList = do
  (nodeScores, nodes) <-
    Monad.foldM
      (\(c1,c2) v -> do
        nodeScore <- score rubric (resources v)
        return (Map.insert (nodeId v) nodeScore c1,
                Map.insert (nodeScore, nodeId v) v c2))
      (Map.empty :: Map.Map i n,
            Map.empty :: Map.Map (n, i) (Node i w r n g))
      nodeList
  return $ RoomBased rubric nodeScores nodes

instance Distributor RoomBased where
  place f (RoomBased ru sc th) = do
    found <- Fold.foldr (findFirstGood f) Nothing th
    newScore <- score ru (resources found)
    oldScore <- Map.lookup (nodeId found) sc
    let id = nodeId found in
      return (RoomBased
                  ru
                  (Map.insert id newScore sc)
                  (Map.insert (newScore, id) found
                    (Map.delete (oldScore, id) th)), found)

data ResourceManager d i w =
  ResourceManager { mgrNodes :: d
                  , mgrAssignments :: Map.Map w i
                  } deriving (Eq, Show)

checkAversions :: (Ord w, Ord r, Ord n, Num n, Ord g)
               => Set.Set g
               -> Workload w r n g
               -> Bool
               -> Bool
checkAversions refGroups (Workload _ _ _ loadGroups) cumul =
  ((Set.size commonGroups) > 0) || cumul
  where
    commonGroups = Set.intersection refGroups loadGroups

hasAverseLoads :: (Ord w, Ord r, Ord n, Num n, Ord g)
               => Workload w r n g
               -> Node i w r n g
               -> Bool
hasAverseLoads (Workload _ _ _ refGroup)
               (Node _ _ nloads) =
  Map.foldr (checkAversions refGroup) False nloads

canAttachWorkload :: (Ord w, Ord r, Ord n, Num n, Ord g)
                  => Workload w r n g
                  -> Node i w r n g
                  -> Bool
canAttachWorkload load node =
  case attachWorkload load node of
    Nothing -> False
    Just _ -> True

attachWorkload :: (Ord w, Ord r, Ord n, Num n, Ord g)
               => Workload w r n g
               -> Node i w r n g
               -> Maybe (Node i w r n g)
attachWorkload load (Node id have attached)
  | Map.size used > Map.size have = Nothing
  | not isAllPositive = Nothing
  | otherwise =
    Just (Node
      id
      leftovers
      (Map.insert (loadId load) load attached))
  where
    -- Subtract what is required from what is available, and only show the
    -- difference for the keys that exist in both
    need = requirements load
    tols = tolerations load
    used = Map.unionWith
                  (-)
                  have
                  need
    -- Check to make sure all that there were in fact enough resources
    -- to meet the requirements' demands
    isAllPositive = Map.foldr
      (\v c -> v >= 0 && c)
      True
      (Map.withoutKeys used tols)
    -- Update values in `have` with values in `used`
    leftovers = Map.unionWith
      (flip const)
      have
      used

attachIfNotAverse :: (Ord w, Ord r, Ord n, Num n, Ord g)
                  => Workload w r n g
                  -> Node i w r n g
                  -> Maybe (Node i w r n g)
attachIfNotAverse load node =
  if hasAverseLoads load node
     then Nothing
    else attachWorkload load node

updateRM :: (Ord i, Ord w, Ord r, Ord n, Num n, Ord g, Distributor d)
         => Map.Map w i
         -> w
         -> d i w r n g
         -> Node i w r n g
         -> ResourceManager (d i w r n g) i w
updateRM
  assignments
  lid
  newNodes
  foundNode =
    ResourceManager
          newNodes $
          Map.insert
            lid
            (nodeId foundNode)
            assignments

assignWorkload :: (Ord i, Ord w, Ord r, Ord n, Num n, Ord g, Distributor d)
               => ResourceManager (d i w r n g) i w
               -> Workload w r n g
               -> Maybe (ResourceManager (d i w r n g) i w)
assignWorkload (ResourceManager nodes assignments) load =
  case place (attachIfNotAverse load) nodes of
    Nothing -> do
      (newNodes, foundNode) <- place (attachWorkload load) nodes
      return $
        updateRM
          assignments
          (loadId load)
          newNodes
          foundNode
    Just (newNodes, foundNode) -> return $
      updateRM
        assignments
        (loadId load)
        newNodes
        foundNode

-- Sort each workload based on its score against a particular
-- rubric in descending order, biggest workloads first.
-- This function is intended to be used in the bin packing
-- algorithm.
sortWorkloads :: (Ord w, Ord r, Ord n, Num n, Ord g)
              => [Workload w r n g]
              -> Map.Map r n
              -> [Workload w r n g]
sortWorkloads workloads rubric = sortOn
  (\a ->
    case score (requirements a) rubric of
      Nothing -> (fromInteger 0)
      Just x -> (negate x)
    )
  workloads

assignWorkloads :: (Ord i, Ord w, Ord r, Ord n, Num n, Ord g, Distributor d)
                => ResourceManager (d i w r n g) i w
                -> [Workload w r n g]
                -> Maybe (ResourceManager (d i w r n g) i w)
assignWorkloads = Monad.foldM assignWorkload
