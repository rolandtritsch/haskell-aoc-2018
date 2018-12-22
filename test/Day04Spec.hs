module Day04Spec where

import Text.Megaparsec (parse)
--import Text.Megaparsec.Debug (dbg)
--import Test.Hspec.Megaparsec (shouldParse)
import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import Test.Hspec

import Day04
import qualified Day04.Part1 as D04P1
import qualified Day04.Part2 as D04P2

run :: IO ()
run = hspec $ do
  let testInput = [
        "[1518-11-01 00:00] Guard #10 begins shift",
        "[1518-11-01 00:05] falls asleep",
        "[1518-11-01 00:25] wakes up",
        "[1518-11-01 00:30] falls asleep",
        "[1518-11-01 00:55] wakes up",
        "[1518-11-01 23:58] Guard #99 begins shift",
        "[1518-11-02 00:40] falls asleep",
        "[1518-11-02 00:50] wakes up",
        "[1518-11-03 00:05] Guard #10 begins shift",
        "[1518-11-03 00:24] falls asleep",
        "[1518-11-03 00:29] wakes up",
        "[1518-11-04 00:02] Guard #99 begins shift",
        "[1518-11-04 00:36] falls asleep",
        "[1518-11-04 00:46] wakes up",
        "[1518-11-05 00:03] Guard #99 begins shift",
        "[1518-11-05 00:45] falls asleep",
        "[1518-11-05 00:55] wakes up        ]"
        ]

  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` StartShift 1033 (Date "1518-02-10" 23 47)
      head parsedInput `shouldBe` StartShift 1033 (Date "1518-02-10" 23 47)

  describe "parse" $ do
    it "should parse the input" $ do
      parse parseDate "" "[1518-11-01 23:58] " `shouldParse` Date "1518-11-01" 23 58
      parse parseShiftStart "" "[1518-11-01 23:58] Guard #99 begins shift" `shouldParse` StartShift 99 (Date "1518-11-01" 23 58)
      parse parseStateChange "" "[1518-11-02 00:40] falls asleep" `shouldParse` StateChange (Date "1518-11-02" 0 40)
      parse parseStateChange "" "[1518-11-02 00:50] wakes up" `shouldParse` StateChange (Date "1518-11-02" 0 50)
      parse parseEvent "" "[1518-11-01 23:58] Guard #99 begins shift" `shouldParse` StartShift 99 (Date "1518-11-01" 23 58)
      parse parseEvent "" "[1518-11-02 00:40] falls asleep" `shouldParse` StateChange (Date "1518-11-02" 0 40)
      parse parseEvent "" "[1518-11-02 00:50] wakes up" `shouldParse` StateChange (Date "1518-11-02" 0 50)
      parse parseEvents "" input1 `parseSatisfies` ((==) 1110 . length)

  describe "stream2Record" $ do
    it "should process the input (for the testcase(s))" $ do
      let records = (stream2Record . input2Stream) testInput
      records !! 0 `shouldBe` Record 10 (Date "1518-11-01" 0 0) 0 Awake

  describe "solve - Part1" $ do
    it "should solve the puzzle" $ do
      D04P1.solve parsedInput `shouldBe` 131469

  describe "solve - Part2" $ do
    it "should solve the puzzle" $ do
      D04P2.solve parsedInput `shouldBe` 96951
