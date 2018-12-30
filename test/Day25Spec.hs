module Day25Spec where

import Text.Megaparsec (parse)
--import Text.Megaparsec.Debug (dbg)
import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import Test.Hspec

import Util (inputParser)

import Day25
import qualified Day25.Part1 as D25P1
import qualified Day25.Part2 as D25P2

run :: IO ()
run = hspec $ do
  let parsedTest0 = inputParser parsePoints "input/Day25test0.txt"
  let parsedTest1 = inputParser parsePoints "input/Day25test1.txt"
  let parsedTest2 = inputParser parsePoints "input/Day25test2.txt"
  let parsedTest3 = inputParser parsePoints "input/Day25test3.txt"

  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` "4,1,7,-2"
      last input `shouldBe` "-8,-7,2,-7"
      head parsedInput `shouldBe` (4,1,7,-2)
      last parsedInput `shouldBe` (-8,-7,2,-7)

  describe "parse" $ do
    it "should parse the input" $ do
      parse parsePoint "" "-8,-7,2,-7" `shouldParse` (-8,-7,2,-7)
      parse parsePoints "" input1 `parseSatisfies` ((==) 1080 . length)

  describe "distance" $ do
    it "should calc the distance correctly" $ do
      distance (0,0,0,0) (3,0,0,0) `shouldBe` 3
      distance (0,3,0,0) (0,0,3,0) `shouldBe` 6
      distance (0,0,0,6) (0,0,0,9) `shouldBe` 3

  describe "edges" $ do
    it "should calc/get correct edges" $ do
      edges parsedTest1 `shouldBe` [((),(3,0,0,0),[(3,0,2,-1),(3,0,0,0)]),((),(0,2,1,-2),[(0,0,2,-2),(0,0,0,-2),(0,2,1,-2)]),((),(-1,0,-1,0),[(-1,2,0,0),(-1,0,-1,0)]),((),(-1,3,2,2),[(-1,2,2,0),(-1,3,2,2)]),((),(3,0,2,-1),[(3,0,2,-1),(3,0,0,0)]),((),(-2,-2,-2,2),[(-2,-2,-2,2)]),((),(-1,2,0,0),[(-1,2,2,0),(-1,2,0,0),(-1,0,-1,0)]),((),(0,0,0,-2),[(0,0,2,-2),(0,0,0,-2),(0,2,1,-2)]),((),(0,0,2,-2),[(0,0,2,-2),(0,0,0,-2),(0,2,1,-2)]),((),(-1,2,2,0),[(-1,2,2,0),(-1,2,0,0),(-1,3,2,2)])]
      edges parsedTest2 `shouldBe` [((),(3,2,0,2),[(3,2,-1,0),(3,2,0,2)]),((),(1,-1,0,-1),[(1,-1,0,1),(0,0,-1,-1),(2,-2,0,-1),(1,-1,0,-1)]),((),(2,-2,0,-1),[(2,-2,0,-1),(1,-1,0,-1)]),((),(-2,2,0,0),[(-2,2,0,0)]),((),(2,3,-2,0),[(3,2,-1,0),(2,3,-2,0)]),((),(0,0,-1,-1),[(2,0,-1,0),(0,0,-1,-1),(1,-1,0,-1)]),((),(0,0,3,1),[(0,0,3,1)]),((),(3,2,-1,0),[(2,0,-1,0),(3,2,-1,0),(2,3,-2,0),(3,2,0,2)]),((),(2,0,-1,0),[(2,0,-1,0),(3,2,-1,0),(0,0,-1,-1)]),((),(1,-1,0,1),[(1,-1,0,1),(1,-1,0,-1)])]

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D25P1.solve parsedTest0 `shouldBe` 2
      D25P1.solve parsedTest1 `shouldBe` 4
      D25P1.solve parsedTest2 `shouldBe` 3
      D25P1.solve parsedTest3 `shouldBe` 8

    it "should solve the puzzle" $ do
      D25P1.solve parsedInput `shouldBe` 428

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D25P2.solve [] `shouldBe` 2

    it "should solve the puzzle" $ do
      D25P2.solve parsedInput `shouldBe` 2
