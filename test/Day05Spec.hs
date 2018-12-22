module Day05Spec where

import Text.Megaparsec (parse)
--import Text.Megaparsec.Debug (dbg)
import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import Test.Hspec

import Util (inputParser1)

import Day05
import qualified Day05.Part1 as D05P1
import qualified Day05.Part2 as D05P2

run :: IO ()
run = hspec $ do
  let testInput = "dabAcCaCBAcCcaDA"

  describe "input" $ do
    it "should return the input" $ do
      take 5 input `shouldBe` [Unit 'c' Plus,Unit 'c' Minus,Unit 'v' Minus,Unit 'v' Plus,Unit 'e' Minus]

  describe "buildPolymer" $ do
    it "should return the list of units (for the testcase(s))" $ do
      buildPolymer testInput `shouldBe` [Unit 'd' Minus,Unit 'a' Minus,Unit 'b' Minus,Unit 'a' Plus,Unit 'c' Minus,Unit 'c' Plus,Unit 'a' Minus,Unit 'c' Plus,Unit 'b' Plus,Unit 'a' Plus,Unit 'c' Minus,Unit 'c' Plus,Unit 'c' Minus,Unit 'a' Minus,Unit 'd' Plus,Unit 'a' Plus]

  describe "parse" $ do
    it "should parse the input" $ do
      parse parsePositiveUnit "" "A" `shouldParse` Unit 'a' Plus
      parse parseNegativeUnit "" "a" `shouldParse` Unit 'a' Minus
      parse parsePolymer "" input1 `parseSatisfies` ((==) 50000 . length)

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      let p = inputParser1 parsePolymer (testInput ++ "\n")
      D05P1.solve p `shouldBe` 10

    it "should solve the puzzle" $ do
      D05P1.solve parsedInput `shouldBe` 11546

  describe "solve - Part2" $ do
    it "should solve the puzzle" $ do
      D05P2.solve parsedInput `shouldBe` 5124
