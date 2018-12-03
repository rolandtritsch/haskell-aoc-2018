module Day03Spec where

import qualified Data.Map as M

import Test.Hspec

import Day03
import qualified Day03.Part1 as D03P1
import qualified Day03.Part2 as D03P2
run :: IO ()
run = hspec $ do
  let testInput = [
        Claim 1 (1,3) (4,4),
        Claim 2 (3,1) (4,4),
        Claim 3 (5,5) (2,2)
        ]

  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` Claim 1 (335,861) (14,10)

  describe "claim" $ do
    it "should produce the correct fabric (for the testcase(s))" $ do
      foldl claim M.empty testInput `shouldBe` M.fromList [((1,3),([1],1)),((1,4),([1],1)),((1,5),([1],1)),((1,6),([1],1)),((2,3),([1],1)),((2,4),([1],1)),((2,5),([1],1)),((2,6),([1],1)),((3,1),([2],1)),((3,2),([2],1)),((3,3),([1,2],2)),((3,4),([1,2],2)),((3,5),([1],1)),((3,6),([1],1)),((4,1),([2],1)),((4,2),([2],1)),((4,3),([1,2],2)),((4,4),([1,2],2)),((4,5),([1],1)),((4,6),([1],1)),((5,1),([2],1)),((5,2),([2],1)),((5,3),([2],1)),((5,4),([2],1)),((5,5),([3],1)),((5,6),([3],1)),((6,1),([2],1)),((6,2),([2],1)),((6,3),([2],1)),((6,4),([2],1)),((6,5),([3],1)),((6,6),([3],1))]

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D03P1.solve testInput `shouldBe` 4

    it "should solve the puzzle" $ do
      D03P1.solve input `shouldBe` 103806

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D03P2.solve testInput `shouldBe` 3

    it "should solve the puzzle" $ do
      D03P2.solve input `shouldBe` 625
