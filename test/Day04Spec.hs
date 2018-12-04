module Day04Spec where

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
      head input `shouldBe` "[1518-02-10 23:47] Guard #1033 begins shift"

  describe "processInput" $ do
    it "should process the input (for the testcase(s))" $ do
      let records = processInput testInput
      records !! 0 `shouldBe` Record 10 "1518-11-01" [False,False,False,False,False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False,False,False,False,False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False,False,False,False,False]

  describe "solve - Part1" $ do
    it "should solve the puzzle" $ do
      D04P1.solve input `shouldBe` 131469

  describe "solve - Part2" $ do
    it "should solve the puzzle" $ do
      D04P2.solve input `shouldBe` 96951
