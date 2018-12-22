module Day04.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day04

-- | solve the puzzle
solve :: Events -> Int
solve es = gid * minute where
  (gid, (minute, _)) = mostAsleepSameMinute $ (record2Shift . stream2Record) es

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day04: Repose Record: Part2: mostAsleepGidMinute -> (%d, %f)\n" result time
