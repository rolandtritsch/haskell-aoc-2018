module Day24Spec where

import Text.Megaparsec (parse)
import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import Test.Hspec

import Util (inputParser)

import Day24
import qualified Day24.Part1 as D24P1
import qualified Day24.Part2 as D24P2

run :: IO ()
run = hspec $ do
  let testInput0 = inputParser parseArmys "input/Day24test0.txt"

  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` "Immune System:"
      last input `shouldBe` "4054 units each with 51210 hit points (immune to radiation, cold, fire) with an attack that does 22 cold damage at initiative 12"
      head (fst parsedInput) `shouldBe` Group WhiteBloodCells 2321 10326 42 Fire 4 [] [Slashing]
      head (snd parsedInput) `shouldBe` Group Viruses 1758 23776 24 Radiation 2 [] []

  describe "parse" $ do
    it "should parse the program" $ do
      parse (parseGroup Viruses) "" "5088 units each with 7917 hit points (weak to slashing; immune to bludgeoning, fire, radiation) with an attack that does 15 fire damage at initiative 17" `shouldParse` Group Viruses 5088 7917 15 Fire 17 [Slashing] [Bludgeoning, Fire, Radiation]
      parse parseArmys "" input1 `parseSatisfies` ((==) 10 . length . fst)
      parse parseArmys "" input1 `parseSatisfies` ((==) 10 . length . snd)

  describe "selectTargets" $ do
    it "should return the targets for the given armys" $ do
      selectTargets testInput0 `shouldBe` [(Group {gType = Viruses, gUnits = 801, gHitpoints = 4706, gAttackDamage = 116, gAttackType = Bludgeoning, gInitiativeLevel = 1, gWeakTo = [Radiation], gImmuneTo = []},Just (Group {gType = WhiteBloodCells, gUnits = 17, gHitpoints = 5390, gAttackDamage = 4507, gAttackType = Fire, gInitiativeLevel = 2, gWeakTo = [Radiation,Bludgeoning], gImmuneTo = []})),(Group {gType = WhiteBloodCells, gUnits = 17, gHitpoints = 5390, gAttackDamage = 4507, gAttackType = Fire, gInitiativeLevel = 2, gWeakTo = [Radiation,Bludgeoning], gImmuneTo = []},Just (Group {gType = Viruses, gUnits = 4485, gHitpoints = 2961, gAttackDamage = 12, gAttackType = Slashing, gInitiativeLevel = 4, gWeakTo = [Fire,Cold], gImmuneTo = [Radiation]})),(Group {gType = Viruses, gUnits = 4485, gHitpoints = 2961, gAttackDamage = 12, gAttackType = Slashing, gInitiativeLevel = 4, gWeakTo = [Fire,Cold], gImmuneTo = [Radiation]},Just (Group {gType = WhiteBloodCells, gUnits = 989, gHitpoints = 1274, gAttackDamage = 25, gAttackType = Slashing, gInitiativeLevel = 3, gWeakTo = [Bludgeoning,Slashing], gImmuneTo = [Fire]})),(Group {gType = WhiteBloodCells, gUnits = 989, gHitpoints = 1274, gAttackDamage = 25, gAttackType = Slashing, gInitiativeLevel = 3, gWeakTo = [Bludgeoning,Slashing], gImmuneTo = [Fire]},Just (Group {gType = Viruses, gUnits = 801, gHitpoints = 4706, gAttackDamage = 116, gAttackType = Bludgeoning, gInitiativeLevel = 1, gWeakTo = [Radiation], gImmuneTo = []}))]

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
