module Day00Spec where

import Text.Megaparsec (parse)
--import Text.Megaparsec.Debug (dbg)
import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import Test.Hspec

import Day00
import qualified Day00.Part1 as D00P1
import qualified Day00.Part2 as D00P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` "Hello"
      last input `shouldBe` "World"

  describe "parse" $ do
    it "should parse the input" $ do
      parse parseWord "" "Roland" `shouldParse` "Roland"
      parse parseWords "" input1 `parseSatisfies` ((==) 2 . length)

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D00P1.solve [] `shouldBe` 1

    it "should solve the puzzle" $ do
      D00P1.solve input `shouldBe` 1

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D00P2.solve [] `shouldBe` 2

    it "should solve the puzzle" $ do
      D00P2.solve input `shouldBe` 2
