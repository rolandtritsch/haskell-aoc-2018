module Day07Spec where

import Test.Hspec

import Day07
import qualified Day07.Part1 as D07P1
import qualified Day07.Part2 as D07P2

run :: IO ()
run = hspec $ do
  let testInput = [('C','A'),('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')]

  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` ('J','E')
      last input `shouldBe` ('E','C')

  describe "findRoots" $ do
    it "should return the roots (for the testcase(s))" $ do
      findRoots (buildGraph testInput) `shouldBe` "C"

    it "should return the roots" $ do
      findRoots (buildGraph input) `shouldBe` "JKX"

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D07P1.solve testInput `shouldBe` "CABDFE"

    it "should solve the puzzle" $ do
      D07P1.solve input `shouldBe` "JDEKPFABTUHOQSXVYMLZCNIGRW"

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D07P2.solve [] `shouldBe` 2

    it "should solve the puzzle" $ do
      D07P2.solve input `shouldBe` 2
