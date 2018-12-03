module TestDay4 where

import Test.Hspec
import Day4

main :: IO ()
main = hspec $ do
  describe "overlap" $ do
    it "should run" $ do
      4 `shouldBe` 4