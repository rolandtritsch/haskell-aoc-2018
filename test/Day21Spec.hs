module Day21Spec where

import Text.Megaparsec (parse)
import Test.Hspec.Megaparsec (parseSatisfies)

import Test.Hspec

--import Day19 (parseProgram, Instruction(Seti), Register)
import Day19 (parseProgram)

import Day21
import qualified Day21.Part1 as D21P1
import qualified Day21.Part2 as D21P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` "#ip 2"
      last input `shouldBe` "seti 5 3 2"
      fst parsedInput `shouldBe` 2
      --last (snd parsedInput) `shouldBe` Seti (Register 5) 3 2

  describe "parse" $ do
    it "should parse the program" $ do
      parse parseProgram "" input1 `parseSatisfies` ((==) 31 . length . snd)

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D21P1.solve (0, []) `shouldBe` 1

    it "should solve the puzzle" $ do
      D21P1.solve parsedInput `shouldBe` 1

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D21P2.solve (0, []) `shouldBe` 2

    it "should solve the puzzle" $ do
      D21P2.solve parsedInput `shouldBe` 2
