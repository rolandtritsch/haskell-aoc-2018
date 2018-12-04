module Day04.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day04

-- | solve the puzzle
solve :: [String] -> Int
solve rs = gid * minute where
  (gid, (minute, _)) = mostAsleepGid $ processInput rs

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day04: Part2: Repose Record: solve -> (%d, %f)\n" result time
