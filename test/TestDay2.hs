module TestDay2 where

import Test.Hspec
import Day2

main :: IO()
main = hspec $ do
  describe "testLen" $ do
    it "finds 2 character examples" $ do
      testLen 2 "abcdef" `shouldBe` False
      testLen 2 "bababc" `shouldBe` True
      testLen 2 "abbcde" `shouldBe` True
      testLen 2 "abcccd" `shouldBe` False
      testLen 2 "aabcdd" `shouldBe` True
      testLen 2 "abcdee" `shouldBe` True
      testLen 2 "ababab" `shouldBe` False
    it "finds 3 character examples" $ do
      testLen 3 "abcdef" `shouldBe` False
      testLen 3 "bababc" `shouldBe` True
      testLen 3 "abbcde" `shouldBe` False
      testLen 3 "abcccd" `shouldBe` True
      testLen 3 "aabcdd" `shouldBe` False
      testLen 3 "abcdee" `shouldBe` False
      testLen 3 "ababab" `shouldBe` True
  describe "difference" $ do
    it "finds the diff between strings" $ do
      difference "abcde" "axcye" `shouldBe` 2
      difference "fghij" "fguij" `shouldBe` 1