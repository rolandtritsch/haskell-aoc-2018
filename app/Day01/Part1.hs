module Day01.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day01

-- | solve the puzzle
solve :: Frequencies -> Int
solve fs = foldl (+) 0 fs

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day01: Chronal Calibration: Part1: sum -> (%d, %f)\n" result time
