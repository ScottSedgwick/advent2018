module TestDay6 where

import Test.Hspec
import Day6
  
main :: IO ()
main = hspec $ do
  describe "distance" $ do
    it "should test" $ do
      distance (1,1) (2,2) `shouldBe` 2
      distance (2,2) (1,1) `shouldBe` 2
  describe "encode" $ do
    it "should reversibly encode points" $ do
      (1,1) `shouldBe` (decodePt $ encodePt (1,1) )
      (100,1) `shouldBe` (decodePt $ encodePt (100,1) )
      (1,100) `shouldBe` (decodePt $ encodePt (1,100) )
      (100,100) `shouldBe` (decodePt $ encodePt (100,100) )


