module TestDay06 where

import Test.Hspec
import Day06
  
main :: IO ()
main = hspec $ do
  describe "distance" $ 
    it "should test" $ do
      distance 1001 2002 `shouldBe` 2
      distance 2002 1001 `shouldBe` 2
  describe "encode" $ 
    it "should reversibly encode points" $ do
      (1,1) `shouldBe` (decodePt $ encodePt (1,1) )
      (100,1) `shouldBe` (decodePt $ encodePt (100,1) )
      (1,100) `shouldBe` (decodePt $ encodePt (1,100) )
      (100,100) `shouldBe` (decodePt $ encodePt (100,100) )


