module Day22Spec where

import Test.Hspec

import Day22
import qualified Day22.Part1 as D22P1
import qualified Day22.Part2 as D22P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` "depth: 4080"
      last input `shouldBe` "target: 14,785"
      parsedInput `shouldBe` (4080, (14, 785))

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D22P1.solve (0, (0, 0)) `shouldBe` 1

    it "should solve the puzzle" $ do
      D22P1.solve parsedInput `shouldBe` 1

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D22P2.solve (0, (0, 0)) `shouldBe` 2

    it "should solve the puzzle" $ do
      D22P2.solve parsedInput `shouldBe` 2
