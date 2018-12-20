module Day17Spec where

import Text.Megaparsec (parse)
import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import Test.Hspec

import Day17
import qualified Day17.Part1 as D17P1
import qualified Day17.Part2 as D17P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` "x=732, y=143..151"
      last input `shouldBe` "y=908, x=545..554"

  describe "parse" $ do
    it "should parse veins" $ do
      parse parseHorizontalVein "" "x=732, y=143..151\n" `shouldParse` Vein (732, 143) (732, 151)
      parse parseVerticalVein "" "y=908, x=545..554\n" `shouldParse` Vein (545, 908) (554, 908)
      parse parseVeins "" input1 `parseSatisfies` ((==) 1713 . length)

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D17P1.solve [] `shouldBe` 1

    it "should solve the puzzle" $ do
      D17P1.solve input `shouldBe` 1

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D17P2.solve [] `shouldBe` 2

    it "should solve the puzzle" $ do
      D17P2.solve input `shouldBe` 2
