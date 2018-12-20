module Day16Spec where

import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import Text.Megaparsec (parse)

import Day16
import qualified Day16.Part1 as D16P1
import qualified Day16.Part2 as D16P2

run :: IO ()
run = hspec $ do
  describe "input1" $ do
    it "should return the input" $ do
      head input1 `shouldBe` "Before: [0, 1, 2, 1]"
      last input1 `shouldBe` "After:  [1, 2, 0, 2]"

  describe "parseRegisters" $ do
    it "should parse the registers" $ do
      parse parseRegisters "" "[9, 10, -1]" `shouldParse` [9,10,-1]

  describe "parseInstruction" $ do
    it "should parse the instruction" $ do
      parse parseInstruction "" "10 11 12 13" `shouldParse` Instruction 10 11 12 13

  describe "parseObservation" $ do
    it "should parse an observation" $ do
      parse parseObservation "" (unlines $ take 3 input1) `shouldParse` Observation [0,1,2,1] (Instruction 14 1 3 3) [0,1,2,1]

  describe "parseObservations" $ do
    it "should parse an observations" $ do
      parse parseObservations "" ((unlines $ input1) ++ "\n") `parseSatisfies` ((==) 790 . length)

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D16P1.solve [] `shouldBe` 1

    it "should solve the puzzle" $ do
      D16P1.solve input1 `shouldBe` 1

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D16P2.solve [] `shouldBe` 2

    it "should solve the puzzle" $ do
      D16P2.solve input2 `shouldBe` 2
