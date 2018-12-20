module Day15Spec where

import qualified Data.Map as M

import Test.Hspec

import Day15
import qualified Day15.Part1 as D15P1
import qualified Day15.Part2 as D15P2

run :: IO ()
run = hspec $ do
  let testInput = [
        "#######",
        "#E..G.#",
        "#...#.#",
        "#.G.#G#",
        "#######"
        ]
  let testBg@(testFields, testUnits) = initialBattleground testInput

  let testInput2 = [
        "#######",
        "#.E...#",
        "#.....#",
        "#...G.#",
        "#######"
        ]
  let testBg2 = initialBattleground testInput2
{-
  let testInput3 = [
        "#########",
        "#G..G..G#",
        "#.......#",
        "#.......#",
        "#G..E..G#",
        "#.......#",
        "#.......#",
        "#G..G..G#",
        "#########"
        ]
  let testBg3 = initialBattleground testInput3
-}
  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` "################################"
      last input `shouldBe` "################################"

  describe "prepare" $ do
    it "should return the battleground for the testcases" $ do
      testFields `shouldBe` M.fromList [
        ((0,0),Wall),((0,1),Wall),((0,2),Wall),((0,3),Wall),((0,4),Wall),((0,5),Wall),((0,6),Wall),
        ((1,0),Wall),((1,1),Open),((1,2),Open),((1,3),Open),((1,4),Open),((1,5),Open),((1,6),Wall),
        ((2,0),Wall),((2,1),Open),((2,2),Open),((2,3),Open),((2,4),Wall),((2,5),Open),((2,6),Wall),
        ((3,0),Wall),((3,1),Open),((3,2),Open),((3,3),Open),((3,4),Wall),((3,5),Open),((3,6),Wall),
        ((4,0),Wall),((4,1),Wall),((4,2),Wall),((4,3),Wall),((4,4),Wall),((4,5),Wall),((4,6),Wall)
        ]
      testUnits `shouldBe` M.fromList [((1,1),Unit Elf 3 200),((1,4),Unit Goblin 3 200),((3,2),Unit Goblin 3 200),((3,5),Unit Goblin 3 200)]

    it "should return the battleground" $ do
      (snd $ initialBattleground input) M.! (2,11) `shouldBe` Unit Goblin 3 200
      (snd $ initialBattleground input) M.! (5,22) `shouldBe` Unit Elf 3 200

  describe "move" $ do
    it "should do the right move for the testcases" $ do
      move testBg M.empty (1,1) (Unit Elf 3 200) `shouldBe` M.fromList [((1,2), (Unit Elf 3 200))]
      move testBg2 M.empty (1,2) (Unit Elf 3 200) `shouldBe` M.fromList [((1,3), (Unit Elf 3 200))]
{-
  describe "nextRound" $ do
    it "should do the right next round for the testcases" $ do
      --let (_, r0) = (iterate nextRound testBg3) !! 0
      --r0 `shouldBe` M.fromList [((1,1),Unit Goblin 3 200),((1,4),Unit Goblin 3 200),((1,7),Unit Goblin 3 200),((4,1),Unit Goblin 3 200),((4,4),Unit Elf 3 200),((4,7),Unit Goblin 3 200),((7,1),Unit Goblin 3 200),((7,4),Unit Goblin 3 200),((7,7),Unit Goblin 3 200)]

      let (_, r1) = (iterate nextRound testBg3) !! 1
      r1 `shouldBe` M.fromList []
-}
  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D15P1.solve [] `shouldBe` 1

    it "should solve the puzzle" $ do
      D15P1.solve input `shouldBe` 1

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D15P2.solve [] `shouldBe` 2

    it "should solve the puzzle" $ do
      D15P2.solve input `shouldBe` 2
