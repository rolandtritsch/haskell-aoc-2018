module Day01.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day01

-- | solve the puzzle
solve :: [Frequency] -> Integer
solve _ = 2

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day01: Part2: solve -> (%f, %d)\n" time result
