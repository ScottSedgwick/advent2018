module TestDay6 where

  import Test.Hspec
  import Day6
  
  main :: IO ()
  main = hspec $ do
    describe "day6" $ do
      it "should test" $ do
        0 `shouldBe` 0