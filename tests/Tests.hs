module Main where

import qualified Data.Map as Map
import Lighthouse
import Test.Hspec

main :: IO ()

main = hspec $
  describe "standard case" $
    it "is supposed to register new workloads" $
      Lighthouse.assignWorkload nodes req `shouldBe` returned
      where
        firstNode = Lighthouse.Node "first"
            (Map.fromList [("cpu", 4.0), ("mem", 8.0)])
            []
        secondNode = Lighthouse.Node
            "second"
            (Map.fromList [("cpu", 2.0), ("mem", 4.0)])
            []
        firstNodeModified = Lighthouse.Node "first"
            (Map.fromList [("cpu", 0.2),
                          ("mem", 5.6)])
            [req]
        nodes = [firstNode, secondNode]
        returned = Just [firstNodeModified, secondNode]
        req = Lighthouse.Workload "myleia"
                        (Map.fromList [("cpu", 3.8), ("mem", 2.4)])
