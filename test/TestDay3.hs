module TestDay3 where

import Test.Hspec
import Lib

mainDay3 :: IO()
mainDay3 = hspec $ do
  describe "testing" $ do
    it "should run" $ do
      d3a1 `shouldBe` 0
      d3a2 `shouldBe` 0