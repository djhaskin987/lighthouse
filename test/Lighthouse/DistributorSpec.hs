{-# LANGUAGE OverloadedStrings #-}
module Lighthouse.DistributorSpec (spec) where

import           Data.Text        (Text, pack)
import           Lighthouse
import           Lighthouse.TestUtilities
import           Test.Hspec
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "prioritized nodes" $
      it "should assign all to the first node" $
          Lighthouse.assignWorkloads startPrioritizedMgr reqs
             `shouldBe` resultPrioritizedMgr
    describe "round robin nodes" $
      it "should assign first to first, second to second" $
          Lighthouse.assignWorkloads startRRMgr reqs
             `shouldBe` resultRRMgr
    describe "room based nodes" $ do
      it "should assign all to second node when sorted asc" $
          Lighthouse.assignWorkloads startRBAscMgr reqs
             `shouldBe` resultRBAscMgr
      it "should assign all to first node when sorted desc" $
          Lighthouse.assignWorkloads startRBDescMgr reqs
             `shouldBe` resultRBDescMgr
  where
    firstNode = Lighthouse.Node
      "firstN"
      (Map.fromList [("cpu", 40), ("mem", 80)])
      Map.empty
    secondNode = Lighthouse.Node
      "secondN"
      (Map.fromList [("cpu", 35), ("mem", 40)])
      Map.empty
    nodes = [firstNode, secondNode]
    startPrioritizedMgr = Lighthouse.ResourceManager
      (Lighthouse.fromListPR nodes)
      Map.empty
    startRRMgr = Lighthouse.ResourceManager
      (Lighthouse.fromListRR nodes)
      Map.empty
    rubricRBAsc = Map.fromList [("cpu", 1), ("mem", 1)]
    startRBAscMgr = Lighthouse.ResourceManager
      (begoneMaybe
        (Lighthouse.fromListRB
          rubricRBAsc
          nodes))
      Map.empty
    rubricRBDesc = Map.fromList [("cpu", -1), ("mem", -1)]
    startRBDescMgr = Lighthouse.ResourceManager
      (begoneMaybe
        (Lighthouse.fromListRB
          rubricRBDesc
          nodes))
      Map.empty
    reqs = [firstReq, secondReq]
    firstReq = Lighthouse.makeSimpleWorkload "first" $
      Map.fromList [("cpu", 13), ("mem", 11)]
    secondReq = Lighthouse.makeSimpleWorkload "second" $
      Map.fromList [("cpu", 18), ("mem", 24)]
    firstNodeAll = Lighthouse.Node
      "firstN"
      (Map.fromList [("cpu", 9), ("mem", 45)])
      (Map.fromList [("first", firstReq),
                     ("second", secondReq)])
    firstNodePrioritized = firstNodeAll
    secondNodePrioritized = secondNode
    resultPrioritizedMgr = Just $ Lighthouse.ResourceManager
      (Lighthouse.fromListPR
        [firstNodePrioritized,
         secondNodePrioritized])
      (Map.fromList [("first","firstN"),
                     ("second","firstN")])
    firstNodeRR = Lighthouse.Node
      "firstN"
      (Map.fromList [("cpu", 27), ("mem", 69)])
      (Map.fromList [("first", firstReq)])
    secondNodeRR = Lighthouse.Node
      "secondN"
      (Map.fromList [("cpu", 17), ("mem", 16)])
      (Map.fromList [("second", secondReq)])
    resultRRMgr = Just $ Lighthouse.ResourceManager
      (Lighthouse.fromListRR [firstNodeRR, secondNodeRR])
      (Map.fromList [("first","firstN"),
                     ("second","secondN")])
    firstNodeRBAsc = firstNode
    secondNodeRBAsc = Lighthouse.Node
      "secondN"
      (Map.fromList [("cpu", 4), ("mem", 5)])
      (Map.fromList [("first", firstReq),
                     ("second", secondReq)])
    resultRBAscMgr = Just $ Lighthouse.ResourceManager
      (begoneMaybe (Lighthouse.fromListRB
                      rubricRBAsc
                      [firstNodeRBAsc, secondNodeRBAsc]))
      (Map.fromList [("first","secondN"),
                     ("second", "secondN")])
    firstNodeRBDesc = firstNodeAll
    secondNodeRBDesc = secondNode
    resultRBDescMgr = Just $ Lighthouse.ResourceManager
      (begoneMaybe (Lighthouse.fromListRB
                      rubricRBDesc
                      [firstNodeRBDesc, secondNodeRBDesc]))
      (Map.fromList [("first","firstN"),
                     ("second", "firstN")])
