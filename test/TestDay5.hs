module TestDay5 where

  import Test.Hspec
  import Day5
  
  main :: IO ()
  main = hspec $ do
    describe "day5" $ do
      it "should test" $ do
        5 `shouldBe` 5