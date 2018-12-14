module Day13Spec where

import qualified Data.Map as M

import Util (inputRaw)

import Test.Hspec

import Day13
import qualified Day13.Part1 as D13P1
import qualified Day13.Part2 as D13P2

run :: IO ()
run = hspec $ do
  let testInput = inputRaw "input/Day13test.txt"
  let (testGrid, testCarts) = buildGrid testInput

  let testInput2 = inputRaw "input/Day13test2.txt"

  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` "                                          /------------\\ /-------------------------------------------------------------------------------\\            "
      last input `shouldBe` "                                 \\--------------------------------------->------------------------------------------------------------/               "

  describe "buildGrid" $ do
    it "should return the grid and the carts (for the testcase(s))" $ do
      M.take 5 testGrid `shouldBe` M.fromList [(Position 0 0,TurnSlash),(Position 0 1,Horizontal),(Position 0 2,Horizontal),(Position 0 3,Horizontal),(Position 0 4,TurnBackslash)]
      M.take 5 testCarts `shouldBe` M.fromList [(Position 0 2,(Right',0)),(Position 3 9,(Down',0))]

    it "should return the grid and the carts" $ do
      let (grid, carts) = buildGrid input
      M.take 5 grid `shouldBe` M.fromList [(Position 0 42,TurnSlash),(Position 0 43,Horizontal),(Position 0 44,Horizontal),(Position 0 45,Horizontal),(Position 0 46,Horizontal)]
      M.take 5 carts `shouldBe` M.fromList [(Position 4 49,(Right',0)),(Position 13 142,(Down',0)),(Position 19 22,(Up',0)),(Position 46 61,(Down',0)),(Position 59 0,(Down',0))]

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D13P1.solve testInput `shouldBe` Position 3 7

    it "should solve the puzzle" $ do
      D13P1.solve input `shouldBe` Position 36 136

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D13P2.solve testInput2 `shouldBe` Position 4 6

    it "should solve the puzzle" $ do
      D13P2.solve input `shouldBe` Position 8 93 -- wrong answer
