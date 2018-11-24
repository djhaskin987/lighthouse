{-# LANGUAGE OverloadedStrings #-}
module Lighthouse.AssignWorkloadSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Lighthouse

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
          (Lighthouse.assignWorkload normalResMgr req) `shouldBe` resultResMgr
  where
    emptyResMgr = Lighthouse.ResourceManager
      []
      Map.empty
    normalResMgr = Lighthouse.ResourceManager
      nodes
      Map.empty
    resultResMgr = Just $ Lighthouse.ResourceManager
      [firstNodeModified, secondNode]
      (Map.fromList [("good","first")])
    firstNode = Lighthouse.Node
      "first"
      (Map.fromList [("cpu", 40), ("mem", 80)])
      Map.empty
    secondNode = Lighthouse.Node
      "second"
      (Map.fromList [("cpu", 20), ("mem", 40)])
      Map.empty
    firstNodeModified = Lighthouse.Node
      "first"
      (Map.fromList [("cpu", 02), ("mem", 56)])
      (Map.fromList [("good", req)])
    nodes = [firstNode, secondNode]
    sillyReq = Lighthouse.Workload
      "bad"
      (Map.fromList [("cpu", 88), ("mem", 164)])
    req = Lighthouse.Workload
      "good"
      (Map.fromList [("cpu", 38), ("mem", 24)])
