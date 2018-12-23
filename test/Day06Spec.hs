module Day06Spec where

import Text.Megaparsec (parse)
--import Text.Megaparsec.Debug (dbg)
import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import qualified Data.Map as M

import Test.Hspec

import Day06
import qualified Day06.Part1 as D06P1
import qualified Day06.Part2 as D06P2

run :: IO ()
run = hspec $ do
  let testInput = [
        (1,1),
        (6,1),
        (3,8),
        (4,3),
        (5,5),
        (9,8)
        ]

  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` (353,177)
      head parsedInput `shouldBe` (353,177)

  describe "parse" $ do
    it "should parse the input" $ do
      parse parseOrigin "" "123, 456" `shouldParse` (123, 456)
      parse parseOrigins "" input1 `parseSatisfies` ((==) 50 . length)

  describe "buildGrid" $ do
    it "should return the grid" $ do
      snd (buildGrid (Boundary 0 9 0 9) testInput) `shouldBe` M.fromList [
        ((0,0),(1,1)),((0,1),(1,1)),((0,2),(1,1)),((0,3),(1,1)),((0,4),(1,1)),              ((0,6),(3,8)),((0,7),(3,8)),((0,8),(3,8)),((0,9),(3,8)),
        ((1,0),(1,1)),((1,1),(1,1)),((1,2),(1,1)),((1,3),(1,1)),((1,4),(1,1)),              ((1,6),(3,8)),((1,7),(3,8)),((1,8),(3,8)),((1,9),(3,8)),
        ((2,0),(1,1)),((2,1),(1,1)),((2,2),(1,1)),((2,3),(4,3)),((2,4),(4,3)),((2,5),(5,5)),((2,6),(3,8)),((2,7),(3,8)),((2,8),(3,8)),((2,9),(3,8)),
        ((3,0),(1,1)),((3,1),(1,1)),((3,2),(4,3)),((3,3),(4,3)),((3,4),(4,3)),((3,5),(5,5)),((3,6),(3,8)),((3,7),(3,8)),((3,8),(3,8)),((3,9),(3,8)),
                                    ((4,2),(4,3)),((4,3),(4,3)),((4,4),(4,3)),((4,5),(5,5)),((4,6),(5,5)),((4,7),(3,8)),((4,8),(3,8)),((4,9),(3,8)),
        ((5,0),(6,1)),((5,1),(6,1)),              ((5,3),(4,3)),((5,4),(5,5)),((5,5),(5,5)),((5,6),(5,5)),((5,7),(5,5)),((5,8),(3,8)),((5,9),(3,8)),
        ((6,0),(6,1)),((6,1),(6,1)),((6,2),(6,1)),              ((6,4),(5,5)),((6,5),(5,5)),((6,6),(5,5)),((6,7),(5,5)),
        ((7,0),(6,1)),((7,1),(6,1)),((7,2),(6,1)),              ((7,4),(5,5)),((7,5),(5,5)),((7,6),(5,5)),((7,7),(9,8)),((7,8),(9,8)),((7,9),(9,8)),
        ((8,0),(6,1)),((8,1),(6,1)),((8,2),(6,1)),              ((8,4),(5,5)),((8,5),(5,5)),((8,6),(9,8)),((8,7),(9,8)),((8,8),(9,8)),((8,9),(9,8)),
        ((9,0),(6,1)),((9,1),(6,1)),((9,2),(6,1)),              ((9,4),(9,8)),((9,5),(9,8)),((9,6),(9,8)),((9,7),(9,8)),((9,8),(9,8)),((9,9),(9,8))
        ]

  describe "infinite" $ do
    it "should return the right origins" $ do
      infinite (buildGrid (Boundary 0 9 0 9) testInput) `shouldBe` [(9,8),(6,1),(3,8),(1,1)]

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D06P1.solve testInput `shouldBe` 17

    it "should solve the puzzle" $ do
      D06P1.solve parsedInput `shouldBe` 4829

  describe "solve - Part2" $ do
    it "should solve the puzzle" $ do
      D06P2.solve parsedInput `shouldBe` 46966
