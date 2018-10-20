module Lighthouse.PlaceSpec (spec) where

import Test.Hspec

import Lighthouse

spec :: Spec
spec =
    describe "normal place" $ do
      it "will die" $
        Lighthouse.place a [1,2,3,4] `shouldBe` Nothing
      it "will find 5 and place it" $
        Lighthouse.place a [1,2,3,4,5] `shouldBe` Just [1,2,3,4,88]
      it "will not find 5" $
        Lighthouse.place a [1,2,3,4] `shouldBe` Nothing
      it "will yield nothing" $
        Lighthouse.place a [] `shouldBe` Nothing
      it "will only place at the first" $
        Lighthouse.place a [5,5,5,5] `shouldBe` Just [88,5,5,5]
  where
    a 5 = Just 88
    a x = Nothing
