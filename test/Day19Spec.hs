module Day19Spec where

import Text.Megaparsec (parse)
import Test.Hspec.Megaparsec (parseSatisfies)

import Test.Hspec

import Day19
import qualified Day19.Part1 as D19P1
import qualified Day19.Part2 as D19P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` "#ip 4"
      last input `shouldBe` "seti 0 7 4"

  describe "parse" $ do
    it "should parse the program" $ do
      parse parseProgram "" input1 `parseSatisfies` ((==) 36 . length . snd)

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D19P1.solve [] `shouldBe` 1

    it "should solve the puzzle" $ do
      D19P1.solve input `shouldBe` 1

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D19P2.solve [] `shouldBe` 2

    it "should solve the puzzle" $ do
      D19P2.solve input `shouldBe` 2
