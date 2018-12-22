module Day02Spec where

import Text.Megaparsec (parse)
--import Text.Megaparsec.Debug (dbg)
import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import Test.Hspec

import Day02
import qualified Day02.Part1 as D02P1
import qualified Day02.Part2 as D02P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` "xrecqmdonskvzupalfkwhjctdb"
      head parsedInput `shouldBe` "xrecqmdonskvzupalfkwhjctdb"

  describe "parse" $ do
    it "should parse the input" $ do
      parse parseBoxId "" (head input) `shouldParse` "xrecqmdonskvzupalfkwhjctdb"
      parse parseBoxIds "" input1 `parseSatisfies` ((==) 250 . length)

  describe "solve - Part1" $ do
    let testInput = [
          "abcdef",
          "bababc",
          "abbcde",
          "abcccd",
          "aabcdd",
          "abcdee",
          "ababab"
          ]
    it "should return the right result(s) for the testcases" $ do
      D02P1.solve testInput `shouldBe` 12

    it "should solve the puzzle" $ do
      D02P1.solve parsedInput `shouldBe` 5976

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      let testInput = [
            "abcde",
            "fghij",
            "klmno",
            "pqrst",
            "fguij",
            "axcye",
            "wvxyz"
            ]
      D02P2.solve testInput `shouldBe` "fgij"

    it "should solve the puzzle" $ do
      D02P2.solve parsedInput `shouldBe` "xretqmmonskvzupalfiwhcfdb"
