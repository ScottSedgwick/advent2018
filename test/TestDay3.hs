module TestDay3 where

import Test.Hspec
import Day3

main :: IO()
main = hspec $ do
  describe "overlap" $ do
    it "should run" $ do
      4 `shouldBe` 4