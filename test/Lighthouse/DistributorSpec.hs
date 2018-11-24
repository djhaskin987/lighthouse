{-# LANGUAGE OverloadedStrings #-}
module Lighthouse.DistributorSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Lighthouse


spec :: Spec
spec = do
    describe "prioritized nodes" $
      it "is supposed to return empty list" $
          Lighthouse.assignWorkloads startPrioritizedMgr reqs
            `shouldBe` resultPrioritizedMgr
    describe "round robin nodes" $
      it "is supposed to return nothing" $
          Lighthouse.assignWorkloads startRRMgr reqs 
             `shouldBe` resultRRMgr
  where
    firstNode = Lighthouse.Node
      "firstN"
      (Map.fromList [("cpu", 40), ("mem", 80)])
      Map.empty
    secondNode = Lighthouse.Node
      "secondN"
      (Map.fromList [("cpu", 20), ("mem", 40)])
      Map.empty
    nodes = [firstNode, secondNode]
    startPrioritizedMgr = Lighthouse.ResourceManager
      nodes
      Map.empty
    startRRMgr = Lighthouse.ResourceManager
      (Lighthouse.fromListRR nodes)
      Map.empty
    reqs = [firstReq, secondReq]
    firstReq = Lighthouse.Workload "first" $
      Map.fromList [("cpu", 13), ("mem", 11)]
    secondReq = Lighthouse.Workload "second" $
      Map.fromList [("cpu", 18), ("mem", 24)]
    firstNodePrioritized = Lighthouse.Node
      "firstN"
      (Map.fromList [("cpu", 9), ("mem", 45)])
      (Map.fromList [("first", firstReq),
                     ("second", secondReq)])
    secondNodePrioritized = secondNode
    resultPrioritizedMgr = Just $ Lighthouse.ResourceManager
      [firstNodePrioritized, secondNodePrioritized]
      (Map.fromList [("first","firstN"), ("second","firstN")])
    firstNodeRR = Lighthouse.Node
      "firstN"
      (Map.fromList [("cpu", 27), ("mem", 69)])
      (Map.fromList [("first", firstReq)])
    secondNodeRR = Lighthouse.Node
      "secondN"
      (Map.fromList [("cpu", 2), ("mem", 16)])
      (Map.fromList [("second", secondReq)])
    resultRRMgr = Just $ Lighthouse.ResourceManager
      (Lighthouse.fromListRR [firstNodeRR, secondNodeRR])
      (Map.fromList [("first","firstN"),
       ("second","secondN")])
