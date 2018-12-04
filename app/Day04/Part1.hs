module Day04.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day04

-- | solve the puzzle
solve :: [String] -> Int
solve rs = maxCid * bestMinute where
  records = processInput rs
  (maxCid, _) = mostAsleep records
  (bestMinute, _) = asleepMost records maxCid

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day04: Part1: Repose Record: solve -> (%d, %f)\n" result time
