module Main where

import qualified Data.Map as Map
import Lighthouse
import Test.Hspec
import qualified Control.Monad as Monad

nearEqNode a b =
    nodeId a == nodeId b &&
    workloads a == workloads b &&
    Map.size resourcesClose == Map.size (resources a) &&
    Map.size resourcesClose == Map.size (resources b) &&
    allResourcesClose
  where
    resourcesClose = Map.intersectionWith
      (\a b -> (a - 0.005) <= b && b <= (a + 0.005))
      (resources a)
      (resources b)
    allResourcesClose = Map.foldr (&&) True resourcesClose

nearEqNodeList a b =
    foldr (&&) True bmps
  where
    cmps = zip a b
    bmps = map (\(s,t) -> s `nearEqNode` t) cmps

nearEqAssignWorkload a b =
  case a of
    Nothing -> case b of
                 Nothing -> True
                 Just v -> False
    Just u -> case b of
                Nothing -> False
                Just v -> nearEqNodeList u v

testPlace = hspec $
    describe "normal place" $ do
      it "will die" $
        Lighthouse.place a [1,2,3,4] `shouldBe` Nothing
      it "will find 5 and place it" $
        Lighthouse.place a [1,2,3,4,5] `shouldBe` Just [1,2,3,4,88]
  where
    a 5 = Just 88
    a x = Nothing

testAssignWorkload = hspec $ do
    describe "empty nodes" $
      it "is supposed to return empty list" $
          Lighthouse.assignWorkload [] req `shouldBe` Nothing
    describe "standard case" $
      it "will do the normal thing" $
        shouldSatisfy
          (Lighthouse.assignWorkload nodes req)
          (nearEqAssignWorkload target)
  where
    firstNode = Lighthouse.Node
      "first"
      (Map.fromList [("cpu", 4.0), ("mem", 8.0)])
      []
    secondNode = Lighthouse.Node
      "second"
      (Map.fromList [("cpu", 2.0), ("mem", 4.0)])
      []
    firstNodeModified = Lighthouse.Node
      "first"
      (Map.fromList [("cpu", 0.2), ("mem", 5.6)])
      [req]
    nodes = [firstNode, secondNode]
    target = Just [firstNodeModified, secondNode]
    req = Lighthouse.Workload
      "myleia"
      (Map.fromList [("cpu", 3.8), ("mem", 2.4)])

main :: IO ()
main = do
    testAssignWorkload
    testPlace
