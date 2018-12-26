module Day24Spec where

import Text.Megaparsec (parse)
import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import Test.Hspec

import Day24
import qualified Day24.Part1 as D24P1
import qualified Day24.Part2 as D24P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` "Immune System:"
      last input `shouldBe` "4054 units each with 51210 hit points (immune to radiation, cold, fire) with an attack that does 22 cold damage at initiative 12"
      head (fst parsedInput) `shouldBe` SoldierGroup 2321 10326 42 Fire 4 [] [Slashing]
      head (snd parsedInput) `shouldBe` SoldierGroup 1758 23776 24 Radiation 2 [] []

  describe "parse" $ do
    it "should parse the program" $ do
      parse parseSoldierGroup "" "5088 units each with 7917 hit points (weak to slashing; immune to bludgeoning, fire, radiation) with an attack that does 15 fire damage at initiative 17" `shouldParse` SoldierGroup 5088 7917 15 Fire 17 [Slashing] [Bludgeoning, Fire, Radiation]
      parse parseArmys "" input1 `parseSatisfies` ((==) 10 . length . fst)
      parse parseArmys "" input1 `parseSatisfies` ((==) 10 . length . snd)

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D24P1.solve ([], []) `shouldBe` 1

    it "should solve the puzzle" $ do
      D24P1.solve parsedInput `shouldBe` 1

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D24P2.solve ([], []) `shouldBe` 2

    it "should solve the puzzle" $ do
      D24P2.solve parsedInput `shouldBe` 2
