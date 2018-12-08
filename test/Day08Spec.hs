module Day08Spec where

import Test.Hspec

import Day08
import qualified Day08.Part1 as D08P1
import qualified Day08.Part2 as D08P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should return the input" $ do
      take 5 input `shouldBe` [7,11,7,2,5]

  describe "solve - Part1" $ do
    it "should solve the puzzle" $ do
      D08P1.solve input `shouldBe` 36566

  describe "solve - Part2" $ do
    it "should solve the puzzle" $ do
      D08P2.solve input `shouldBe` 30548
