module Day01Spec where

import Test.Hspec

import Day01
import qualified Day01.Part1 as D01P1
import qualified Day01.Part2 as D01P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should return the input" $ do
      take 5 input `shouldBe` [9,1,-11,12,17]

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D01P1.solve [1, -2, 3, 1] `shouldBe` 3

    it "should solve the puzzle" $ do
      D01P1.solve input `shouldBe` 590

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D01P2.solve [] `shouldBe` 2

    it "should solve the puzzle" $ do
      D01P2.solve input `shouldBe` 2
