module Day09Spec where

import qualified Data.Map as M
import qualified Data.Sequence as S

import Test.Hspec

import Day09
import qualified Day09.Part1 as D09P1
import qualified Day09.Part2 as D09P2

run :: IO ()
run = hspec $ do
  let testInput = (9, 25)

  describe "input" $ do
    it "should return the input" $ do
      input `shouldBe` (431, 70950)

  describe "move" $ do
    it "should produce the correct game state (for the testcase(s))" $ do
      let state22 = GameState (9, 25) M.empty 3 (13, 22, S.fromList [0,16,8,17,4,18,9,19,2,20,10,21,5,22,11,1,12,6,13,3,14,7,15])
      let state23 = GameState (9, 25) (M.fromList [(4, 32)]) 4 (6, 23, S.fromList [0,16,8,17,4,18,19,2,20,10,21,5,22,11,1,12,6,13,3,14,7,15])
      let state25 = GameState (9, 25) (M.fromList [(4, 32)]) 6 (10, 25, S.fromList [0,16,8,17,4,18,19,2,24,20,25,10,21,5,22,11,1,12,6,13,3,14,7,15])
      iterate addMarple (initGame testInput) !! 21 `shouldBe` state22
      iterate addMarple (initGame testInput) !! 22 `shouldBe` state23
      iterate addMarple (initGame testInput) !! 24 `shouldBe` state25

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D09P1.solve testInput `shouldBe` 32
      D09P1.solve (1,48) `shouldBe` 95
      D09P1.solve (9,48) `shouldBe` 63
      D09P1.solve (10,1618) `shouldBe` 8317
      D09P1.solve (13,7999) `shouldBe` 146373
      D09P1.solve (17,1104) `shouldBe` 2764
      D09P1.solve (21,6111) `shouldBe` 54718
      D09P1.solve (30,5807) `shouldBe` 37305

    it "should solve the puzzle" $ do
      D09P1.solve input `shouldBe` 404611

  describe "solve - Part2" $ do
    it "should solve the puzzle" $ do
      D09P2.solve input `shouldBe` 3350093681
