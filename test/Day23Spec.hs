module Day23Spec where

import Text.Megaparsec (parse)
import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import Test.Hspec

import Day23
import qualified Day23.Part1 as D23P1
import qualified Day23.Part2 as D23P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` "pos=<-39857152,26545464,51505035>, r=86328482"
      last input `shouldBe` "pos=<-25332390,55010041,15159102>, r=64903159"
      head parsedInput `shouldBe` NanoBot (-39857152,26545464,51505035) 86328482
      last parsedInput  `shouldBe` NanoBot (-25332390,55010041,15159102) 64903159

  describe "parse" $ do
    it "should parse the program" $ do
      parse parseNanoBot "" "pos=<-39857152,26545464,51505035>, r=86328482" `shouldParse` NanoBot (-39857152,26545464,51505035) 86328482
      parse parseNanoBots "" input1 `parseSatisfies` ((==) 1000 . length)

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D23P1.solve [] `shouldBe` 1

    it "should solve the puzzle" $ do
      D23P1.solve parsedInput `shouldBe` 1

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D23P2.solve [] `shouldBe` 2

    it "should solve the puzzle" $ do
      D23P2.solve parsedInput `shouldBe` 2
