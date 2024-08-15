{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Halyard
Description : Workload scheduler
Copyright   : (c) The Halyard Authors, 2024
                  See the AUTHORS.md file.
License     : BSD
Maintainer  : djhaskin987@gmail.com
Stability   : experimental

Halyard is a workload scheduling application.
-}

module Halyard (
                  ) where

class JoinableTree t where
  unjoin :: () -> (t, t)
  join :: t -> t

class IndexedTree t where
  insert :: (v -> Int64 -> Bool) -> v -> t
  remove :: (v -> Int64 -> Bool) -> v -> t
  lookup :: (v -> Int64 -> Bool) -> t
  glue :: v -> t -> t
  unglue :: () -> (t, v, t)
  split :: (v -> Int64 -> Bool) -> (t, t)
  size :: () -> Int64
  minElement :: () -> Maybe v
  maxElement :: () -> Maybe v

data WeightBalancedPennantNode =
    Leaf
  | TreeNode v Int64 WeightBlanacedPennantNode WeightBalancedPennantNode
  deriving (Show, Eq)

data WeightBalancedPennantTree v =
    Empty
  | MinPennant v Int64 WeightBalancedPennantNode
  | MaxPennant v Int64 WeightBalancedPennantNode
  deriving (Show, Eq)

-- Now instance out IndexedTree with WBPNode
-- After that, instance IndexedTree and JoinableTree with WBPTree


-- |The basic unit of work that Halyard knows about is a Workload.
-- Workloads have an ID, a list of requirements and quantity associated
-- with each requirement, a list of tolerations and a list of aversion groups.
-- The purpose for each of these is covered more in depth in the documentation
-- of the function 'attachWorkload'.
data Workload w r n g =
  Workload {
           -- |Returns the id associated with the workload.
             loadId :: w
           -- |Returns the requirements associated with the workload.
           , requirements :: Map.Map r n
           -- |Returns the set of tolerations associated with the workload.
           , tolerations :: Set.Set r
           -- |Returns the aversion groups associated with the workload.
           , aversionGroups :: Set.Set g
           } deriving (Show, Eq, Generic)

-- TODO: Get rid of this.
makeSimpleWorkload :: w -> Map.Map r n -> Workload w r n g
makeSimpleWorkload id reqs =
  Workload
    id
    reqs
    (Set.empty :: Set.Set r)
    (Set.empty :: Set.Set g)

-- |The basic unit of worker that Halyard knows about is a Node.
-- Nodes have an ID, a list of resources and a quantity associated with each
-- resource. It also keeps track of which workloads have been previously
-- assigned to it.
data Node i w r n g =
  Node {
         -- |Returns the id associated with the node.
         nodeId :: i
         -- |Returns the resources associated with the node.
       , resources :: Map.Map r n
         -- |Returns the assigned workloads associated with the node.
       , assignedWorkloads :: Map.Map w (Workload w r n g)
       } deriving (Show, Eq, Generic)

-- |The Distributor class represents a collection of nodes, together
-- with a strategy for how to schedule particular workloads onto
-- particular nodes.
-- This strategy is embodied in the 'place' method.
class Distributor d where
  -- |Takes a function which may return a 'Node'. If it does,
  -- return a modified distributor containing the new version of the node
  -- and also return the node containing the newly placed item. In practice,
  -- this item is always a 'Workload'.
  place :: (Ord i, Ord w, Ord r, Ord n, Num n, Ord g)
        => (Node i w r n g -> Maybe (Node i w r n g))
        -> d i w r n g
        -> Maybe (d i w r n g, Node i w r n g)

newtype Prioritized i w r n g =
  Prioritized { prioritizedThings :: Sequence.Seq (Node i w r n g)
              } deriving (Show, Eq)

instance Distributor Prioritized where
  place f (Prioritized Empty) = Nothing
  place f (Prioritized (x:<|xs)) =
    case f x of
      Nothing -> case place f (Prioritized xs) of
                   Nothing -> Nothing
                   Just (Prioritized ys, found) ->
                     Just (Prioritized (x <| ys), found)
      Just y -> Just (Prioritized (y <| xs), y)

-- |Construct a Prioritized distributor suitable for use when constructing a
-- a 'ResourceManager'.
--
-- The Prioritized type of distributor will attempt to place workloads in the
-- order they were received, onto nodes in the order they were given. This is
-- much like the RoundRobin distributor created by the 'fromListRR' function.
-- The difference is that Prioritized will attempt to place each successive
-- load starting again at the beginning of the list of nodes each time.
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

-- |Construct a RoundRobin distributor suitable for use when constructing
-- a 'ResourceManager'.
--
-- The RoundRobin type of distributor will attempt to place workloads in the
-- order they were received, onto nodes in the order they were given. This is
-- much like the 'Prioritized' distributor. The difference is that RoundRobin
-- will attempt to place each successive load starting with nodes further down
-- in the list from the node which just had a succesfull placement, on nodes
-- where placement of the previous workload has not yet been attempted.
fromListRR :: (Ord i, Ord w, Ord r, Ord n, Num n, Ord g)
           => [Node i w r n g]
           -> RoundRobin i w r n g
fromListRR nodeList = RoundRobin $ Sequence.fromList nodeList

findFirstGood  :: (a -> Maybe a) -> a -> Maybe a -> Maybe a
findFirstGood f a c = case f a of
                        Nothing -> c
                        Just y -> Just y

-- |Take a map of requirements or resources
-- and return a single, scalar value "scoring" it.
-- This score is used when sorting workloads and keeping
-- Nodes in sorted order in the 'RoomBased' distributor,
-- which makes an attempt at bin packing.
-- If there are not entries in the scored map corresponding
-- to each entry in the rubric, return @Nothing@.
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

-- |This function behaves the same as the 'score' function, except call 'error'
-- when the rubric's quantities do not all exist in the map instead of
-- returning @Nothing@.
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

-- |Construct a RoomBased distributor suitable for use when constructing a
-- 'ResourceManager'.
-- The RoomBased type of distributor will attempt to sort workloads from
-- the largest size to the smallest, and will also attempt to keep nodes
-- in sorted order from least room to most room at all times.
--
-- In order to sort workloads, it "scores" each
-- workload by taking the quantity associated with each of the 'requirements'
-- in the workload, multiplying it by each quantity associated with that named
-- requirement in the given rubric, and summing together each multiplication to
-- come up with a single number for each workload.
--
-- In order to sort nodes, it "scores" each
-- node by taking the quantity associated with each of the 'resources' in the
-- node, multiplying it by each quantity associated with that named
-- entry in the given rubric, and summing together each multiplication to
-- come up with a single number for each node.
--
-- If there is an entry in the rubric which does not have a corresponding entry
-- in the requirements of all workloads and nodes in the system, @Nothing@ will
-- be returned. To be clear, a "corresponding entry" means that for each
-- key\/value pair in the rubric, a key\/value pair must exist such that the
-- keys are equal in value for all workloads' requirements and all nodes'
-- resources in the system.
--
-- Once scored, the RoomBased distributor will attempt to place nodes in order
-- from largest workload by score to smallest. For each workload, it will try
-- to place the workload beginning at the smallest node by score, and continue
-- to attempt to place the workload at successively larger nodes until the
-- workload is placed. Once placed, the quantities of items required will be
-- subtracted from the entries in the resources of the node to track how much
-- room in the node is left. The remaining resources are then re-scored for the
-- node and the node will be replaced into the list of nodes according to
-- this new score reflecting that it has less room now for a future placement.
-- This is in an attempt to implement something that is
-- roughly like the [/Best Fit
-- Decreasing/](https://en.wikipedia.org/wiki/Bin_packing_problem#Analysis_of_approximate_algorithms)
-- bin packing algorithm.
--
-- __WARNING__: The RoomBased distributor assumes that all score values for
-- nodes and loads are positive, but it allows the user to specify a rubric
-- with negative values in it. It assumes a negative number is a smaller value,
-- so don't try to use negative numbers to change sorting order. If you negate
-- all the values in the rubric in an attempt at load balancing, it will break
-- bin packing. Nodes with a score of higher magnitude (a large negative
-- number) will be sorted before nodes with a lower magnitude negative score,
-- and workloads with higher magnitude negative number will be sorted after
-- nodes with lower magnitude score.
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

-- |A ResourceManager keeps track of nodes and assignments made
-- throughout the life of an assignment process.
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
  (Set.size commonGroups > 0) || cumul
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

-- |This method attempts to attach a 'Workload' onto a 'Node'.
-- First it subtracts each item in the node's 'resources' by the
-- quantity attached to that item in the workload's 'requirements'.
-- Then it takes all items in the node's resources list and checks
-- to make sure that their quantities are all greater than 0. Any
-- quantity found to be less than zero will result in a @Nothing@ returned
-- by the function.
--
-- The exception to this is any item name found in the
-- 'tolerations' of the workload. Any negative items named in the tolerations
-- set of the workload which are found in the subtraction
-- of requirements of the workload from the resources of the node will be
-- ignored and attachment of the workload can complete as normal.
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

-- |This is like 'attachWorkload', but this function also honors any
-- aversions found in the 'aversionGroups' of the workload.
-- An aversionGroup is a set of names. If any workload that has already
-- been assigned to this node has a name in its aversionGroups set that is also
-- in the 'aversionGroups' set of the workload, Nothing is returned. Otherwise
-- attachment is attempted the same as in the 'attachWorkload' function.
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

-- |Attempt to assign a workload to a node housed within a ResourceManager.
-- If the node could not be assigned (there's no room for it),
-- @Nothing@ is returned.
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

-- |Sort each workload based on its score against a particular
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
      Nothing -> 0
      Just x -> negate x
    )
  workloads

-- |Assign a list of workloads to a ResourceManager.
assignWorkloads :: (Ord i, Ord w, Ord r, Ord n, Num n, Ord g, Distributor d)
                => ResourceManager (d i w r n g) i w
                -> [Workload w r n g]
                -> Maybe (ResourceManager (d i w r n g) i w)
assignWorkloads = Monad.foldM assignWorkload

-- |/TRY/ to assign a list of workloads to a ResourceManager.
-- This means that if a workload could not be assigned,
-- the function moves on to the next workload.
-- This way a workload that could not otherwise be scheduled
-- doesn't ruin the prospects for the rest of the loads.
-- Returns the sequence of workloads which couldn't get
-- scheduled and the resulting resource manager.
tryAssignWorkloads :: (Ord i, Ord w, Ord r, Ord n, Num n, Ord g, Distributor d)
                   => ResourceManager (d i w r n g) i w
                   -> [Workload w r n g]
                   -> (Sequence.Seq (Workload w r n g),
                       ResourceManager (d i w r n g) i w)
tryAssignWorkloads rm lds =
  foldr (\v (bad, mgr) ->
          case assignWorkload mgr v of
            Just mgr' -> (bad, mgr')
            Nothing -> (bad |> v, mgr))
        (Empty, rm)
        lds
