module TestDay07 where

import Test.Hspec
import Day07
  
main :: IO ()
main = hspec $
  describe "distance" $ 
    it "should test" $
      0 `shouldBe` 0