module Day07Spec where

import Text.Megaparsec (parse)
--import Text.Megaparsec.Debug (dbg)
import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import Test.Hspec

import Day07
import qualified Day07.Part1 as D07P1
import qualified Day07.Part2 as D07P2

run :: IO ()
run = hspec $ do
  let testInput = [('C','A'),('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')]

  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` ('J','E')
      last input `shouldBe` ('E','C')
      head parsedInput `shouldBe` ('J','E')
      last parsedInput `shouldBe` ('E','C')

  describe "parse" $ do
    it "should parse the input" $ do
      parse parseDependency "" "Step J must be finished before step E can begin." `shouldParse` ('J', 'E')
      parse parseDependencies "" input1 `parseSatisfies` ((==) 101 . length)

  describe "findRoots" $ do
    it "should return the roots (for the testcase(s))" $ do
      findRoots (buildGraph testInput) `shouldBe` "C"

    it "should return the roots" $ do
      findRoots (buildGraph parsedInput) `shouldBe` "JKX"

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D07P1.solve testInput `shouldBe` "CABDFE"

    it "should solve the puzzle" $ do
      D07P1.solve parsedInput `shouldBe` "JDEKPFABTUHOQSXVYMLZCNIGRW"

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D07P2.solve 0 2 testInput `shouldBe` 15

    it "should solve the puzzle" $ do
      D07P2.solve 60 5 parsedInput `shouldBe` 1048
