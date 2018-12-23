module Day12Spec where

import Text.Megaparsec (parse)
--import Text.Megaparsec.Debug (dbg)
import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import qualified Data.Map as M

import Test.Hspec

import Day12
import qualified Day12.Part1 as D12P1
import qualified Day12.Part2 as D12P2

run :: IO ()
run = hspec $ do
  let testState = M.fromList $ zip [0..] [True,False,False,True,False,True,False,False,True,True,False,False,False,False,False,False,True,True,True,False,False,False,True,True,True]
  let testNotes = M.fromList [
        ([False,False,False,True,True], True),
        ([False,False,True,False,False], True),
        ([False,True,False,False,False], True),
        ([False,True,False,True,False], True),
        ([False,True,False,True,True], True),
        ([False,True,True,False,False], True),
        ([False,True,True,True,True], True),
        ([True,False,True,False,True], True),
        ([True,False,True,True,True], True),
        ([True,True,False,True,False], True),
        ([True,True,False,True,True], True),
        ([True,True,True,False,False], True),
        ([True,True,True,False,True], True),
        ([True,True,True,True,False], True)
        ]
  let testInput = (testState, testNotes)

  describe "input" $ do
    it "should return the input" $ do
      (M.take 5 $ fst input) `shouldBe` M.fromList [(0,True),(1,True),(2,False),(3,True),(4,True)]
      (M.take 5 $ snd input) `shouldBe` M.fromList [([False,False,False,False,False],False),([False,False,False,False,True],False),([False,False,False,True,False],False),([False,False,False,True,True],True),([False,False,True,False,False],False)]
      (M.take 5 $ fst parsedInput) `shouldBe` M.fromList [(0,True),(1,True),(2,False),(3,True),(4,True)]
      (M.take 5 $ snd parsedInput) `shouldBe` M.fromList [([False,False,False,False,False],False),([False,False,False,False,True],False),([False,False,False,True,False],False),([False,False,False,True,True],True),([False,False,True,False,False],False)]

  describe "parse" $ do
    it "should parse the input" $ do
      parse parseState "" "initial state: ##.##.##..#..#.#.#.#...#...#####.###...#####.##..#####.#..#.##..#..#.#...#...##.##...#.##......####." `shouldParse` M.fromList [(0,True),(1,True),(2,False),(3,True),(4,True),(5,False),(6,True),(7,True),(8,False),(9,False),(10,True),(11,False),(12,False),(13,True),(14,False),(15,True),(16,False),(17,True),(18,False),(19,True),(20,False),(21,False),(22,False),(23,True),(24,False),(25,False),(26,False),(27,True),(28,True),(29,True),(30,True),(31,True),(32,False),(33,True),(34,True),(35,True),(36,False),(37,False),(38,False),(39,True),(40,True),(41,True),(42,True),(43,True),(44,False),(45,True),(46,True),(47,False),(48,False),(49,True),(50,True),(51,True),(52,True),(53,True),(54,False),(55,True),(56,False),(57,False),(58,True),(59,False),(60,True),(61,True),(62,False),(63,False),(64,True),(65,False),(66,False),(67,True),(68,False),(69,True),(70,False),(71,False),(72,False),(73,True),(74,False),(75,False),(76,False),(77,True),(78,True),(79,False),(80,True),(81,True),(82,False),(83,False),(84,False),(85,True),(86,False),(87,True),(88,True),(89,False),(90,False),(91,False),(92,False),(93,False),(94,False),(95,True),(96,True),(97,True),(98,True),(99,False)]
      parse parseInit "" input1 `parseSatisfies` ((==) 32 . M.size . snd)

  describe "detectShortcut" $ do
    it "should return the right shortcut" $ do
      detectShortcut 50000000000 (snd input) (fst input) `shouldBe` (5629, 49999999901, 62)

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D12P1.solve testInput `shouldBe` 325

    it "should solve the puzzle" $ do
      D12P1.solve input `shouldBe` 2930

  describe "solve - Part2" $ do
    it "should solve the puzzle" $ do
      D12P2.solve input `shouldBe` 3099999999491
