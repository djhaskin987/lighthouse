{-# LANGUAGE OverloadedStrings #-}
module Lighthouse.AssignWorkloadSpec (spec) where

import Test.Hspec
import qualified Control.Monad as Monad
import qualified Data.Map as Map
import Lighthouse

nearEqNode :: Node -> Node -> Bool
nearEqNode a b =
    nodeId a == nodeId b &&
    Map.size resourcesClose == Map.size (resources a) &&
    Map.size resourcesClose == Map.size (resources b) &&
    allResourcesClose
  where
    resourcesClose = Map.intersectionWith
      (\a b -> (a - 0.005) <= b && b <= (a + 0.005))
      (resources a)
      (resources b)
    allResourcesClose = and resourcesClose

nearEqNodeList :: [Node] -> [Node] -> Bool
nearEqNodeList a b =
    and bmps
  where
    cmps = zip a b
    bmps = map (uncurry nearEqNode) cmps

nearEqAssignResMgr :: Maybe (ResourceManager [] Node)
                   -> Maybe (ResourceManager [] Node)
                   -> Bool
nearEqAssignResMgr a b =
  case a of
    Nothing -> case b of
                 Nothing -> True
                 Just v -> False
    Just (ResourceManager aNs aAs) -> case b of
                Nothing -> True
                Just (ResourceManager bNs bAs) ->
                  ((nearEqNodeList aNs bNs) && aAs == bAs)

spec :: Spec
spec = do
    describe "empty nodes" $
      it "is supposed to return empty list" $
          Lighthouse.assignWorkload emptyResMgr req `shouldBe` Nothing
    describe "silly case" $
      it "is supposed to return nothing" $
          Lighthouse.assignWorkload normalResMgr sillyReq `shouldBe` Nothing
    describe "standard case" $
      it "will do the normal thing" $
        shouldSatisfy
          (Lighthouse.assignWorkload normalResMgr req)
          (nearEqAssignResMgr resultResMgr)
  where
    emptyResMgr = Lighthouse.ResourceManager
      []
      []
    normalResMgr = Lighthouse.ResourceManager
      nodes
      []
    resultResMgr = Just $ Lighthouse.ResourceManager
      [firstNodeModified, secondNode]
      [(Assignment "first" "good")]
    firstNode = Lighthouse.Node
      "first"
      (Map.fromList [("cpu", 4.0), ("mem", 8.0)])
    secondNode = Lighthouse.Node
      "second"
      (Map.fromList [("cpu", 2.0), ("mem", 4.0)])
    firstNodeModified = Lighthouse.Node
      "first"
      (Map.fromList [("cpu", 0.2), ("mem", 5.6)])
    nodes = [firstNode, secondNode]
    sillyReq = Lighthouse.Workload
      "bad"
      (Map.fromList [("cpu", 8.8), ("mem", 16.4)])
    req = Lighthouse.Workload
      "good"
      (Map.fromList [("cpu", 3.8), ("mem", 2.4)])
