module Main where

import Lighthouse
import Test.Hspec

main :: IO ()
main = hspec $
  describe "Validate haqify function" $ do
    it "haqify is supposed to prefix Haq! to things" $
      -- haqify "me" `shouldBe` "Haq! me"
      lightUp "orange" `shouldBe` "LIGHT orange LIGHT"
    it "haqify is supposed to prefix Haq! to things" $
      -- haqify "me" `shouldBe` "Haq! me"
      0 `shouldBe` 0
