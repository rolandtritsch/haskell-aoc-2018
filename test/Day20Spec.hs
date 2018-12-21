module Day20Spec where

import Text.Megaparsec (parse)
--import Text.Megaparsec.Debug (dbg)
import Test.Hspec.Megaparsec (shouldParse)
--import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import Test.Hspec

import Day20
import qualified Day20.Part1 as D20P1
import qualified Day20.Part2 as D20P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should return the input" $ do
      (take 10 $ head input) `shouldBe` "^ESWSWNWWS"

  describe "parse" $ do
    it "should parse the regex/routes" $ do
      parse parsePath "" "NSWE" `shouldParse` Path [North,South,West,East]
      parse parseBranch "" "(NS|WE)" `shouldParse` Branch (Path [North,South]) (Path [West,East])
      parse parseBranch "" "(|WE)" `shouldParse` Branch (Path []) (Path [West,East])
      parse parseBranch "" "(NS|)" `shouldParse` Branch (Path [North,South]) (Path [])
      parse parseRoute "" "NSWE" `shouldParse` Path [North, South, West, East]
      parse parseRoute "" "(NS|WE)" `shouldParse` Branch (Path [North,South]) (Path [West,East])
      --parse (dbg "routes" parseRoutes) "" "^NSWE(NS|WE)NSWE$" `shouldParse` [Path [North, South, West, East], Branch (Path [North,South]) (Path [West,East]), Path [North, South, West, East]]
      --parse parseRoutes "" "^NSWE(N|S)(E|)(|W)NSWE(N(S|(E|W)))NSWE(((N|S)|W)|E)$" `parseSatisfies` ((==) 0 . length)
      --parse parseRoutes "" input1 `parseSatisfies` ((==) 0 . length)

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D20P1.solve [] `shouldBe` 1

    it "should solve the puzzle" $ do
      D20P1.solve input `shouldBe` 1

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D20P2.solve [] `shouldBe` 2

    it "should solve the puzzle" $ do
      D20P2.solve input `shouldBe` 2
