module Day14Spec where

import Test.Hspec

import Day14
import qualified Day14.Part1 as D14P1
import qualified Day14.Part2 as D14P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` "Hello"
      last input `shouldBe` "World"

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D14P1.solve [] `shouldBe` 1

    it "should solve the puzzle" $ do
      D14P1.solve input `shouldBe` 1

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D14P2.solve [] `shouldBe` 2

    it "should solve the puzzle" $ do
      D14P2.solve input `shouldBe` 2
