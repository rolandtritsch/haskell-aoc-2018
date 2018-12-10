module Day11.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day11

-- | solve the puzzle
solve :: [String] -> Integer
solve _ = 1

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day11: Part1: solve -> (%d, %f)\n" result time