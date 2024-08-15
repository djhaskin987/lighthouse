{-# LANGUAGE OverloadedStrings #-}
module Lighthouse.AssignWorkloadSpec (spec) where

import           Data.Text        (Text, pack)
import           Lighthouse
import           Lighthouse.TestUtilities
import           Test.Hspec
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "empty nodes" $ do
      it "is supposed to return empty list PR" $
        Lighthouse.assignWorkload emptyPRMgr req
          `shouldBe` Nothing
      it "is supposed to return empty list RR" $
        Lighthouse.assignWorkload emptyRRMgr req
          `shouldBe` Nothing
      it "is supposed to return empty list RB" $
        Lighthouse.assignWorkload emptyRBMgr req
          `shouldBe` Nothing
    describe "silly case" $ do
      it "is supposed to return nothing PR" $
          Lighthouse.assignWorkload normalPRResMgr sillyReq `shouldBe` Nothing
      it "is supposed to return nothing RR" $
          Lighthouse.assignWorkload normalRRResMgr sillyReq `shouldBe` Nothing
      it "is supposed to return nothing RB" $
          Lighthouse.assignWorkload normalRBResMgr sillyReq `shouldBe` Nothing
    describe "off case" $ do
      it "is supposed to return nothing PR" $
          Lighthouse.assignWorkload normalPRResMgr wrongReq `shouldBe` Nothing
      it "is supposed to return nothing RR" $
          Lighthouse.assignWorkload normalRRResMgr wrongReq `shouldBe` Nothing
      it "is supposed to return nothing RB" $
          Lighthouse.assignWorkload normalRBResMgr wrongReq `shouldBe` Nothing
    describe "vacuous case" $
      it "is supposed to get assigned" $
          Lighthouse.assignWorkload normalPRResMgr vacuousReq
          `shouldBe` resultVacuous
    describe "standard case" $ do
      it "will do the normal thing PR" $
          Lighthouse.assignWorkload normalPRResMgr req
          `shouldBe` resultPRResMgr
      it "will do the normal thing RR" $
          Lighthouse.assignWorkload normalRRResMgr req
          `shouldBe` resultRRResMgr
      it "will do the normal thing RB" $
          Lighthouse.assignWorkload normalRBResMgr req
          `shouldBe` resultRBResMgr
  where
    emptyPRMgr = Lighthouse.ResourceManager
      (fromListPR ([] :: [TestNode]))
      (Map.empty :: Map.Map Text Text)
    emptyRRMgr = Lighthouse.ResourceManager
      (Lighthouse.fromListRR ([] :: [TestNode]))
      (Map.empty :: Map.Map Text Text)
    emptyRBMgr = Lighthouse.ResourceManager
      (begoneMaybe (Lighthouse.fromListRB
                    (Map.fromList [("cpu", -1), ("mem", -1)])
                    ([] :: [TestNode])) :: RoomBased Text Text Text Int Text)
      (Map.empty :: Map.Map Text Text)
    normalPRResMgr = Lighthouse.ResourceManager
      (fromListPR nodes)
      (Map.empty :: Map.Map Text Text)
    normalRRResMgr = Lighthouse.ResourceManager
      (fromListRR nodes)
      (Map.empty :: Map.Map Text Text)
    normalRBResMgr = Lighthouse.ResourceManager
      (begoneMaybe (fromListRB
                    (Map.fromList [("cpu", -1), ("mem", -1)])
                    nodes) :: RoomBased Text Text Text Int Text)
      (Map.empty :: Map.Map Text Text)
    resultPRResMgr = Just $ Lighthouse.ResourceManager
      (fromListPR [firstNodeModified, secondNode])
      (Map.fromList [("good","first")])
    resultRRResMgr = Just $ Lighthouse.ResourceManager
      (fromListRR [secondNode, firstNodeModified])
      (Map.fromList [("good","first")])
    resultRBResMgr = Just $ Lighthouse.ResourceManager
      (begoneMaybe (fromListRB
                    (Map.fromList [("cpu", -1), ("mem", -1)])
                    [firstNodeModified, secondNode]))
      (Map.fromList [("good","first")])
    resultVacuous = Just $ Lighthouse.ResourceManager
      (fromListPR [firstNodeVacuous, secondNode])
      (Map.fromList [("vacuous", "first")])
    firstNode = Lighthouse.Node
      "first"
      (Map.fromList [("cpu", 40), ("mem", 80)])
      (Map.empty :: Map.Map Text TestWorkload)
    secondNode = Lighthouse.Node
      "second"
      (Map.fromList [("cpu", 20), ("mem", 40)])
      (Map.empty :: Map.Map Text TestWorkload)
    firstNodeModified = Lighthouse.Node
      "first"
      (Map.fromList [("cpu", 2), ("mem", 56)])
      (Map.fromList [("good", req)])
    firstNodeVacuous = Lighthouse.Node
      "first"
      (Map.fromList [("cpu", 40), ("mem", 80)])
      (Map.fromList [("vacuous", vacuousReq)]) :: TestNode
    nodes = [firstNode, secondNode]
    sillyReq = Lighthouse.makeSimpleWorkload
      "bad"
      (Map.fromList [("cpu", 88), ("mem", 164)]) :: TestWorkload
    wrongReq = Lighthouse.makeSimpleWorkload
      "off"
      (Map.fromList [("disk", 1)]) :: TestWorkload
    vacuousReq = Lighthouse.makeSimpleWorkload
      "vacuous"
      (Map.fromList ([] :: [(Text,Int)])) :: TestWorkload
    req = Lighthouse.makeSimpleWorkload
      "good"
      (Map.fromList [("cpu", 38), ("mem", 24)]) :: TestWorkload
