module Day12Spec where

import qualified Data.Map as M

import Test.Hspec

import Day12
import qualified Day12.Part1 as D12P1
import qualified Day12.Part2 as D12P2

run :: IO ()
run = hspec $ do
  let testState = M.fromList $ zip [0..] [True,False,False,True,False,True,False,False,True,True,False,False,False,False,False,False,True,True,True,False,False,False,True,True,True]
  let testNotes = M.fromList [
        ([False,False,False,True,True], True),
        ([False,False,True,False,False], True),
        ([False,True,False,False,False], True),
        ([False,True,False,True,False], True),
        ([False,True,False,True,True], True),
        ([False,True,True,False,False], True),
        ([False,True,True,True,True], True),
        ([True,False,True,False,True], True),
        ([True,False,True,True,True], True),
        ([True,True,False,True,False], True),
        ([True,True,False,True,True], True),
        ([True,True,True,False,False], True),
        ([True,True,True,False,True], True),
        ([True,True,True,True,False], True)
        ]
  let testInput = (testState, testNotes)

  describe "input" $ do
    it "should return the input" $ do
      (M.take 5 $ fst input) `shouldBe` M.fromList [(0,True),(1,True),(2,False),(3,True),(4,True)]
      (M.take 5 $ snd input) `shouldBe` M.fromList [([False,False,False,False,False],False),([False,False,False,False,True],False),([False,False,False,True,False],False),([False,False,False,True,True],True),([False,False,True,False,False],False)]

  describe "detectShortcut" $ do
    it "should return the right shortcut" $ do
      detectShortcut 200 (snd input) (fst input) `shouldBe` (5629, 49999999901, 62)

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D12P1.solve testInput `shouldBe` 325

    it "should solve the puzzle" $ do
      D12P1.solve input `shouldBe` 2930

  describe "solve - Part2" $ do
    it "should solve the puzzle" $ do
      D12P2.solve input `shouldBe` 3099999999491
