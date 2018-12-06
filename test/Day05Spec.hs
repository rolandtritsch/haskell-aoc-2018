module Day05Spec where

import Test.Hspec

import Day05
import qualified Day05.Part1 as D05P1
import qualified Day05.Part2 as D05P2

run :: IO ()
run = hspec $ do
  let testInput = "dabAcCaCBAcCcaDA"

  describe "input" $ do
    it "should return the input" $ do
      take 10 input `shouldBe` "CcvVeGgRbB"

  describe "buildPolymer" $ do
    it "should return the list of units (for the testcase(s))" $ do
      buildPolymer testInput `shouldBe` [Unit 'd' Minus,Unit 'a' Minus,Unit 'b' Minus,Unit 'a' Plus,Unit 'c' Minus,Unit 'c' Plus,Unit 'a' Minus,Unit 'c' Plus,Unit 'b' Plus,Unit 'a' Plus,Unit 'c' Minus,Unit 'c' Plus,Unit 'c' Minus,Unit 'a' Minus,Unit 'd' Plus,Unit 'a' Plus]

    it "should return the list of units" $ do
      take 5 (buildPolymer input) `shouldBe` [Unit 'c' Plus,Unit 'c' Minus,Unit 'v' Minus,Unit 'v' Plus,Unit 'e' Minus]

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D05P1.solve testInput `shouldBe` 10

    it "should solve the puzzle" $ do
      D05P1.solve input `shouldBe` 11546

  describe "solve - Part2" $ do
    it "should solve the puzzle" $ do
      D05P2.solve input `shouldBe` 5124
