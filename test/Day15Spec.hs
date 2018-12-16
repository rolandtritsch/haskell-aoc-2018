module Day15Spec where

import qualified Data.Map as M

import Test.Hspec

import Day15
import qualified Day15.Part1 as D15P1
import qualified Day15.Part2 as D15P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` "################################"
      last input `shouldBe` "################################"

  describe "prepare" $ do
    it "should return the battleground" $ do
      (snd $ initialBattleground input) M.! (2,11) `shouldBe` Unit Goblin 3 200
      (snd $ initialBattleground input) M.! (5,22) `shouldBe` Unit Elf 3 200

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D15P1.solve [] `shouldBe` 1

    it "should solve the puzzle" $ do
      D15P1.solve input `shouldBe` 1

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D15P2.solve [] `shouldBe` 2

    it "should solve the puzzle" $ do
      D15P2.solve input `shouldBe` 2
