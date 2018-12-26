module Day25Spec where

import Text.Megaparsec (parse)
--import Text.Megaparsec.Debug (dbg)
import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import Test.Hspec

import Day25
import qualified Day25.Part1 as D25P1
import qualified Day25.Part2 as D25P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` "4,1,7,-2"
      last input `shouldBe` "-8,-7,2,-7"
      head parsedInput `shouldBe` (4,1,7,-2)
      last parsedInput `shouldBe` (-8,-7,2,-7)

  describe "parse" $ do
    it "should parse the input" $ do
      parse parsePoint "" "-8,-7,2,-7" `shouldParse` (-8,-7,2,-7)
      parse parsePoints "" input1 `parseSatisfies` ((==) 1080 . length)

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D25P1.solve [] `shouldBe` 1

    it "should solve the puzzle" $ do
      D25P1.solve parsedInput `shouldBe` 1

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D25P2.solve [] `shouldBe` 2

    it "should solve the puzzle" $ do
      D25P2.solve parsedInput `shouldBe` 2
