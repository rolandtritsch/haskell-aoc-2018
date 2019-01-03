module Day24Spec where

import Text.Megaparsec (parse)
import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import Test.Hspec

import Util (inputParser)

import qualified Data.Map as M

import Day24
import qualified Day24.Part1 as D24P1
import qualified Day24.Part2 as D24P2

run :: IO ()
run = hspec $ do
  let testInput0 = M.fromList $ zip [0..] $ inputParser parseGroups "input/Day24test0.txt"

  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` "Immune System:"
      last input `shouldBe` "4054 units each with 51210 hit points (immune to radiation, cold, fire) with an attack that does 22 cold damage at initiative 12"
      parsedInput M.! 0 `shouldBe` Group WhiteBloodCells 2321 10326 42 Fire 4 [] [Slashing]

  describe "parse" $ do
    it "should parse the program" $ do
      parse (parseGroup Viruses) "" "5088 units each with 7917 hit points (weak to slashing; immune to bludgeoning, fire, radiation) with an attack that does 15 fire damage at initiative 17" `shouldParse` Group Viruses 5088 7917 15 Fire 17 [Slashing] [Bludgeoning, Fire, Radiation]
      parse parseGroups "" input1 `parseSatisfies` ((==) 20 . length)

  describe "selectTargets" $ do
    it "should return the targets for the given armys" $ do
      -- (3,1) (1,2) (0,3) (2,0)
      selectTargets testInput0 `shouldBe` M.fromList [(0,3),(1,2),(2,0),(3,1)]

  describe "fight" $ do
    it "should return the next groups after a (round of a) fight" $ do
      fight testInput0 (selectTargets testInput0) `shouldBe` M.fromList [(1,Group {gType = WhiteBloodCells, gUnits = 905, gHitpoints = 1274, gAttackDamage = 25, gAttackType = Slashing, gInitiativeLevel = 3, gWeakTo = [Bludgeoning,Slashing], gImmuneTo = [Fire]}),(2,Group {gType = Viruses, gUnits = 797, gHitpoints = 4706, gAttackDamage = 116, gAttackType = Bludgeoning, gInitiativeLevel = 1, gWeakTo = [Radiation], gImmuneTo = []}),(3,Group {gType = Viruses, gUnits = 4434, gHitpoints = 2961, gAttackDamage = 12, gAttackType = Slashing, gInitiativeLevel = 4, gWeakTo = [Fire,Cold], gImmuneTo = [Radiation]})]

  describe "combat" $ do
    it "should return the winning groups" $ do
      combat testInput0 `shouldBe` M.fromList [(2,Group {gType = Viruses, gUnits = 782, gHitpoints = 4706, gAttackDamage = 116, gAttackType = Bludgeoning, gInitiativeLevel = 1, gWeakTo = [Radiation], gImmuneTo = []}),(3,Group {gType = Viruses, gUnits = 4434, gHitpoints = 2961, gAttackDamage = 12, gAttackType = Slashing, gInitiativeLevel = 4, gWeakTo = [Fire,Cold], gImmuneTo = [Radiation]})]

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D24P1.solve testInput0 `shouldBe` 5216

    it "should solve the puzzle" $ do
      D24P1.solve parsedInput `shouldBe` 18346

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D24P2.solve M.empty `shouldBe` 2

    it "should solve the puzzle" $ do
      D24P2.solve parsedInput `shouldBe` 2
