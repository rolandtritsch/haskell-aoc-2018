module Day18Spec where

import Text.Megaparsec (parse)
import Test.Hspec.Megaparsec (parseSatisfies)

import Test.Hspec

import Day18
import qualified Day18.Part1 as D18P1
import qualified Day18.Part2 as D18P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` ".||..#.#.#..#..#|...#|....|.|.#.###....##.|#..#..."
      last input `shouldBe` "#|#.##.#.#.|...|....#.###....|..#....#.....|#.||.."

  describe "parse" $ do
    it "should parse the area" $ do
      parse parseArea "" input1 `parseSatisfies` ((==) 2500 . length)

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D18P1.solve [] `shouldBe` 1

    it "should solve the puzzle" $ do
      D18P1.solve parsedInput `shouldBe` 1

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D18P2.solve [] `shouldBe` 2

    it "should solve the puzzle" $ do
      D18P2.solve parsedInput `shouldBe` 2
