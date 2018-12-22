{-# LANGUAGE OverloadedStrings #-}
module Lighthouse.AversionsSpec (spec) where

import           Data.Text        (Text, pack)
import           Lighthouse
import           Test.Hspec
import           Lighthouse.TestUtilities
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "single aversion case" $ do
      it "should still be able to schedule in the presence of an aversion" $
        Lighthouse.assignWorkload onlyPonyMgr singleAvrsReq
          `shouldBe` onlyPonyResult
      it "should schedule in normal order if aversions cannot be met" $
        Lighthouse.assignWorkload crowdedMgr singleAvrsReq
          `shouldBe` crowdedResult
      it "should prefer not to schedule together when aversion is present" $
        Lighthouse.assignWorkload vacantMgr singleAvrsReq
          `shouldBe` vacantResult
    describe "add to the pot" $
      it "should add to the pot" $
        Lighthouse.assignWorkload contributeMgr contributeReq
          `shouldBe` contributeResult
--     describe "multiple aversion case"
--       it "should try not to schedule if any aversion is present" do
--         Lighthouse.assignWorkload startMgr counterExampleReq
--           `shouldBe` Nothing
--       it "should try not to schedule if all aversions are present" do
--         Lighthouse.assignWorkload startMgr counterExampleReq
--           `shouldBe` Nothing
--     describe "toleration counter example" $
--       it "should be unable to schedule a workload" $
--         Lighthouse.assignWorkload startMgr counterExampleReq
--           `shouldBe` Nothing
--     describe "basic toleration case" $
--       it "should be able to schedule a workload" $
--         Lighthouse.assignWorkload startMgr basicExampleReq
--           `shouldBe` endMgr
--     describe "not using tolerations per se" $
--       it "should cancel out a present negative value" $
--         Lighthouse.assignWorkload semaphoreMgr semaphoreReq
--           `shouldBe` semaphoreResult
--     describe "tolerate self-pollution" $
--       it "should tolerate the stuff it causes, but still cause it" $
--         Lighthouse.assignWorkload polluteMgr polluteReq
--           `shouldBe` polluteResult
  where
    singleAvrsReq = defaultWorkload {
      loadId = "singleAvrs",
      requirements = Map.fromList [("cpu", 1), ("mem", 1)],
      aversionGroups = Set.fromList ["spiders"]
                                    }
    secondAvrsReq = defaultWorkload {
      loadId = "secondAvrs",
      requirements = Map.fromList [("cpu", 1), ("mem", 1)],
      aversionGroups = Set.fromList ["spiders"]
                                    }
    thirdAvrsReq = defaultWorkload {
      loadId = "thirdAvrs",
      requirements = Map.fromList [("cpu", 1), ("mem", 1)],
      aversionGroups = Set.fromList ["spiders"]
                                    }
    secondAvrsNode = defaultNode {
      nodeId = "single",
      resources = Map.fromList [("cpu", 1), ("mem", 1)],
      assignedWorkloads = Map.fromList [("secondAvrs",
        secondAvrsReq)]
                                 }
    thirdAvrsNode = defaultNode {
      nodeId = "second",
      resources = Map.fromList [("cpu", 1), ("mem", 1)],
      assignedWorkloads = Map.fromList [("thirdAvrs",
        thirdAvrsReq)]
                                }
    vacantFirstNode = defaultNode {
      nodeId = "vacant1",
      resources = Map.fromList [("cpu", 1), ("mem", 1)],
      assignedWorkloads = (Map.empty :: Map.Map Text TestWorkload)
        }
    onlyPonyResultNode = defaultNode {
      nodeId = "single",
      resources = Map.fromList [("cpu", 0), ("mem", 0)],
      assignedWorkloads = Map.fromList [("secondAvrs",
        secondAvrsReq), ("singleAvrs",
        singleAvrsReq)]
                                     }
    onlyPonyMgr = Lighthouse.ResourceManager
      (Lighthouse.fromListPR [secondAvrsNode])
      Map.empty
    onlyPonyResult = Just $ Lighthouse.ResourceManager
      (Lighthouse.fromListPR [onlyPonyResultNode])
      (Map.fromList [("singleAvrs", "single")])
    crowdedMgr = Lighthouse.ResourceManager
      (Lighthouse.fromListPR [secondAvrsNode, thirdAvrsNode])
      Map.empty
    crowdedResultNode = defaultNode {
      nodeId = "single",
      resources = Map.fromList [("cpu", 0), ("mem", 0)],
      assignedWorkloads = Map.fromList [("secondAvrs",
        secondAvrsReq), ("singleAvrs",
        singleAvrsReq)]
                                     }
    crowdedResult = Just $ Lighthouse.ResourceManager
      (Lighthouse.fromListPR [crowdedResultNode, thirdAvrsNode])
      (Map.fromList [("singleAvrs", "single")])
    vacantMgr = Lighthouse.ResourceManager
      (Lighthouse.fromListPR [secondAvrsNode, vacantFirstNode])
      Map.empty
    vacantResultNode = defaultNode {
      nodeId = "vacant1",
      resources = Map.fromList [("cpu", 0), ("mem", 0)],
      assignedWorkloads = (Map.fromList [("singleAvrs", singleAvrsReq)])
                                   }
    vacantResult = Just $ Lighthouse.ResourceManager
      (Lighthouse.fromListPR [secondAvrsNode, vacantResultNode])
      (Map.fromList [("singleAvrs", "vacant1")])
    contributeNode = Lighthouse.Node
      "contributeN"
      (Map.fromList [("cpu", 10)])
      (Map.empty :: Map.Map Text TestWorkload)
    contributeReq = Lighthouse.makeSimpleWorkload
      "contribute"
      (Map.fromList [("cpu", -10)]) :: TestWorkload
    contributeResultNode = Lighthouse.Node
      "contributeN"
      (Map.fromList [("cpu", 20)])
      (Map.fromList [("contribute", contributeReq)])
    contributeMgr = Lighthouse.ResourceManager
      (Lighthouse.fromListPR [contributeNode])
      Map.empty
    contributeResult = Just $ Lighthouse.ResourceManager
      (Lighthouse.fromListPR [contributeResultNode])
      (Map.fromList [("contribute", "contributeN")])
    polluteNode = Lighthouse.Node
      "polluteN"
      (Map.fromList [("cpu", 10)])
      (Map.empty :: Map.Map Text TestWorkload)
    polluteReq = defaultWorkload {
        loadId = "pollute",
        requirements = Map.fromList [("cpu", 20)],
        tolerations = Set.fromList [("cpu")]
        }
    polluteResultNode = Lighthouse.Node
      "polluteN"
      (Map.fromList [("cpu", -10)])
      (Map.fromList [("pollute", polluteReq)]) :: TestNode
    polluteMgr = Lighthouse.ResourceManager
      (Lighthouse.fromListPR [polluteNode])
      (Map.empty :: Map.Map Text Text)
    polluteResult = Just $ Lighthouse.ResourceManager
      (Lighthouse.fromListPR [polluteResultNode])
      (Map.fromList [("pollute", "polluteN")])
    firstNode = Lighthouse.Node
      "firstN"
      (Map.fromList [("cpu", 40), ("mem", 80), ("master-node", minBound::Int)])
      (Map.empty :: Map.Map Text TestWorkload)
    semaphoreNode = Lighthouse.Node
      "semaphoreN"
      (Map.fromList [("cpu", 10), ("mem", 10), ("semaphore", -3)])
      (Map.empty :: Map.Map Text TestWorkload)
    semaphoreReq = Lighthouse.makeSimpleWorkload
      "semaphore"
      (Map.fromList [("cpu", 10), ("mem", 10), ("semaphore", -3)])
        :: TestWorkload
    semaphoreMgr = Lighthouse.ResourceManager
      (Lighthouse.fromListPR [semaphoreNode])
      (Map.empty :: Map.Map Text Text)
    semaphoreResultNode = Lighthouse.Node
      "semaphoreN"
      (Map.fromList [("cpu", 0), ("mem", 0), ("semaphore", 0)])
      (Map.fromList [("semaphore", semaphoreReq)])
    semaphoreResult = Just $ Lighthouse.ResourceManager
      (Lighthouse.fromListPR [semaphoreResultNode])
      (Map.fromList [("semaphore", "semaphoreN")])
    firstNodeScheduled = Lighthouse.Node
      "firstN"
      (Map.fromList [("cpu", 27), ("mem", 69), ("master-node", minBound::Int)])
      (Map.fromList [("first", basicExampleReq)])
    startMgr = Lighthouse.ResourceManager
      (Lighthouse.fromListPR [firstNode])
      Map.empty
    endMgr = Just $ Lighthouse.ResourceManager
      (Lighthouse.fromListPR [firstNodeScheduled])
      (Map.fromList [("first","firstN")])
    counterExampleReq = Lighthouse.makeSimpleWorkload "first" $
      Map.fromList [("cpu", 13), ("mem", 11)] :: TestWorkload
    basicExampleReq = defaultWorkload {
      loadId = "first",
      requirements = (Map.fromList [("cpu", 13), ("mem", 11)]),
      tolerations = (Set.fromList ["master-node"])
                                      }
