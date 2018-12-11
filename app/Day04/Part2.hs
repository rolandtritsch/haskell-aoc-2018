module Day04.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day04

-- | solve the puzzle
solve :: [String] -> Int
solve rs = gid * minute where
  (gid, (minute, _)) = mostAsleepSameMinute $ (record2Shift . stream2Record . input2Stream) rs

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day04: Repose Record: Part2: mostAsleepGidMinute -> (%d, %f)\n" result time
