module Day04.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day04

-- | solve the puzzle
solve :: [String] -> Int
solve rs = maxCid * bestMinute where
  shifts = (record2Shift . stream2Record . input2Stream) rs
  (maxCid, _) = mostAsleep shifts
  (bestMinute, _) = asleepMost shifts maxCid

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day04: Repose Record: Part1: mostAsleep -> (%d, %f)\n" result time
