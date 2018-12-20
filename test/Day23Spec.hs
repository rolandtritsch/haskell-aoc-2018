module Day23Spec where

import Test.Hspec

import Day23
import qualified Day23.Part1 as D23P1
import qualified Day23.Part2 as D23P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` "Hello"
      last input `shouldBe` "World"

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D23P1.solve [] `shouldBe` 1

    it "should solve the puzzle" $ do
      D23P1.solve input `shouldBe` 1

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D23P2.solve [] `shouldBe` 2

    it "should solve the puzzle" $ do
      D23P2.solve input `shouldBe` 2
