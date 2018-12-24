module Day16Spec where

import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import Text.Megaparsec (parse)

import Day16
import qualified Day16.Part1 as D16P1
import qualified Day16.Part2 as D16P2

run :: IO ()
run = hspec $ do
  describe "parse" $ do
    it "should parse the input" $ do
      parse parseRegisters "" "[9, 10, -1]" `shouldParse` [9,10,-1]
      parse parseInstruction "" "10 11 12 13" `shouldParse` Instruction 10 11 12 13
      parse parseObservation "" "Before: [0, 1, 2, 1]\n14 1 3 3\nAfter:  [0, 1, 2, 1]\n" `shouldParse` Observation [0,1,2,1] (Instruction 14 1 3 3) [0,1,2,1]
      parse parseObservations "Day16input1.txt" input11 `parseSatisfies` ((==) 790 . length)
      parse parseInstructions "Day16input2.txt" input12 `parseSatisfies` ((==) 841 . length)

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D16P1.solve [] `shouldBe` 1

    it "should solve the puzzle" $ do
      D16P1.solve parsedInput1 `shouldBe` 1

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D16P2.solve [] `shouldBe` 2

    it "should solve the puzzle" $ do
      D16P2.solve parsedInput2 `shouldBe` 2
