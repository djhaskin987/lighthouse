module Main where

import qualified Data.Map as Map
import Lighthouse
import Test.Hspec

testPlace =
    describe "empty place" $
      it "will find 5 and place it" $
        Lighthouse.place a [1,2,3,4,5] `shouldBe` Just [1,2,3,4,88]
  where
    a 5 = Just 88
    a x = Nothing

testAssignWorkload = do
    describe "empty nodes" $
        it "is supposed to return empty list" $
            Lighthouse.assignWorkload [] req `shouldBe` Nothing
    describe "standard case" $
        it "is supposed to register new workloads" $
            Lighthouse.assignWorkload nodes req `shouldBe` returned
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
    returned = Just [firstNodeModified, secondNode]
    req = Lighthouse.Workload
      "myleia"
      (Map.fromList [("cpu", 3.8), ("mem", 2.4)])

main :: IO ()
main = hspec $ do
  testAssignWorkload
  testPlace
