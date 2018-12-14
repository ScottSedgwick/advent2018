module TestDay08 where

import Test.Hspec
import Day08
  
main :: IO ()
main = hspec $
  describe "day 08" $ 
    it "should test" $
      0 `shouldBe` 0