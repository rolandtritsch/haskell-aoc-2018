module Day13Spec where

import Text.Megaparsec (parse)
--import Text.Megaparsec.Debug (dbg)
import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import qualified Data.Map as M

import Util (inputParser)

import Test.Hspec

import Day13
import qualified Day13.Part1 as D13P1
import qualified Day13.Part2 as D13P2

run :: IO ()
run = hspec $ do
  let testInput@(testGrid, testCarts) = inputParser parseInit "input/Day13test.txt"
  let testInput2 = inputParser parseInit "input/Day13test2.txt"
  let input2 = inputParser parseInit "input/Day13input2.txt"
  let input3 = inputParser parseInit "input/Day13input3.txt"
  let input4 = inputParser parseInit "input/Day13input4.txt"

  describe "buildGrid" $ do
    it "should return the grid and the carts (for the testcase(s))" $ do
      M.take 5 testGrid `shouldBe` M.fromList [(Position 0 0,TurnSlash),(Position 0 1,Horizontal),(Position 0 2,Horizontal),(Position 0 3,Horizontal),(Position 0 4,TurnBackSlash)]
      M.take 5 testCarts `shouldBe` M.fromList [(Position 0 2,(Right',0)),(Position 3 9,(Down',0))]

    it "should return the grid and the carts" $ do
      let (grid, carts) = input
      M.take 5 grid `shouldBe` M.fromList [(Position 0 42,TurnSlash),(Position 0 43,Horizontal),(Position 0 44,Horizontal),(Position 0 45,Horizontal),(Position 0 46,Horizontal)]
      M.take 5 carts `shouldBe` M.fromList [(Position 4 49,(Right',0)),(Position 13 142,(Down',0)),(Position 19 22,(Up',0)),(Position 46 61,(Down',0)),(Position 59 0,(Down',0))]

      let (grid', carts') = inputParser parseInit "input/Day13input.txt"
      M.take 5 grid' `shouldBe` M.fromList [(Position 0 42,TurnSlash),(Position 0 43,Horizontal),(Position 0 44,Horizontal),(Position 0 45,Horizontal),(Position 0 46,Horizontal)]
      M.take 5 carts' `shouldBe` M.fromList [(Position 4 49,(Right',0)),(Position 13 142,(Down',0)),(Position 19 22,(Up',0)),(Position 46 61,(Down',0)),(Position 59 0,(Down',0))]

  describe "parse" $ do
    it "should parse the input" $ do
      parse parseLine "" "|-/\\+^v<>" `shouldParse` [((Position 0 0,Vertical),Nothing),((Position 0 1,Horizontal),Nothing),((Position 0 2,TurnSlash),Nothing),((Position 0 3,TurnBackSlash),Nothing),((Position 0 4,Intersection),Nothing),((Position 0 5,Vertical),Just (Position 0 5,(Up',0))),((Position 0 6,Vertical),Just (Position 0 6,(Down',0))),((Position 0 7,Horizontal),Just (Position 0 7,(Left',0))),((Position 0 8,Horizontal),Just (Position 0 8,(Right',0)))]
      parse parseInit "" input1 `parseSatisfies` ((==) 15046 . M.size . fst)
      parse parseInit "" input1 `parseSatisfies` ((==) 17 . M.size . snd)

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D13P1.solve testInput `shouldBe` Position 3 7
      D13P1.solve input2 `shouldBe` Position 49 83
      D13P1.solve input3 `shouldBe` Position 138 115
      D13P1.solve input4 `shouldBe` Position 54 50

    it "should solve the puzzle" $ do
      D13P1.solve parsedInput `shouldBe` Position 36 136

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D13P2.solve testInput2 `shouldBe` Position 4 6
      D13P2.solve input4 `shouldBe` Position 100 50

    it "should solve the puzzle" $ do
      D13P2.solve parsedInput `shouldBe` Position 111 53
