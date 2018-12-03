module TestDay3 where

import Test.Hspec
import Day3

mainDay3 :: IO()
mainDay3 = hspec $ do
  describe "overlap" $ do
    it "should run" $ do
      overlap (1,1,3,4,4) (2,3,1,4,4) `shouldBe` 4