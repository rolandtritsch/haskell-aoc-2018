module Day01Spec where

import Text.Megaparsec (parse)
--import Text.Megaparsec.Debug (dbg)
import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import Test.Hspec

import Day01
import qualified Day01.Part1 as D01P1
import qualified Day01.Part2 as D01P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should return the input" $ do
      take 5 input `shouldBe` [9,1,-11,12,17]
      take 5 parsedInput `shouldBe` [9,1,-11,12,17]

  describe "parse" $ do
    it "should parse the input" $ do
      parse parseFrequency "" "+1" `shouldParse` 1
      parse parseFrequency "" "-1" `shouldParse` negate 1
      parse parseFrequencies "" input1 `parseSatisfies` ((==) 1023 . length)

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D01P1.solve [1, -2, 3, 1] `shouldBe` 3

    it "should solve the puzzle" $ do
      D01P1.solve parsedInput `shouldBe` 590

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D01P2.solve [-1, 1] `shouldBe` 0
      D01P2.solve [3, 3, 4, -2, -4] `shouldBe` 10
      D01P2.solve [-6, 3, 8, 5, -6] `shouldBe` 5
      D01P2.solve [7, 7, -2, -7, -4] `shouldBe` 14

    it "should solve the puzzle" $ do
      D01P2.solve parsedInput `shouldBe` 83445
