module Day01.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day01

-- | solve the puzzle
solve :: [Frequency] -> Integer
solve fs = foldl (+) 0 fs

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day01: Part1: Chronal Calibration: sum -> (%d, %f)\n" result time
