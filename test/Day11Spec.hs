module Day11Spec where

import qualified Data.Map as M

import Test.Hspec

import Day11
import qualified Day11.Part1 as D11P1
import qualified Day11.Part2 as D11P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should return the input" $ do
      input `shouldBe` 3999

  describe "buildPowerGrid" $ do
    it "should setup the grid correctly (for the testcase(s))" $ do
      (buildPowerGrid 57) M.! (122,79) `shouldBe` -5
      (buildPowerGrid 39) M.! (217,196) `shouldBe` 0
      (buildPowerGrid 71) M.! (101,153) `shouldBe` 4

  describe "largestTotalPowerLevel" $ do
    it "should return the largest total power level (for the testcase(s))" $ do
      largestTotalPowerLevel 3 (buildPowerGrid 18) `shouldBe` ((33,45),29, 3)
      largestTotalPowerLevel 3 (buildPowerGrid 42) `shouldBe` ((21,61),30, 3)
      largestTotalPowerLevel 300 (buildPowerGrid 18) `shouldBe` ((90,269),113, 16)
      largestTotalPowerLevel 300 (buildPowerGrid 42) `shouldBe` ((232,251),119, 12)

  describe "solve - Part1" $ do
    it "should solve the puzzle" $ do
      D11P1.solve input `shouldBe` (21,77)

  describe "solve - Part2" $ do
    it "should solve the puzzle" $ do
      D11P2.solve input `shouldBe` ((0,0),0)
