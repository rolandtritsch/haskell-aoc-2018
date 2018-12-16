module Day15.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day15

-- | solve the puzzle
solve :: [String] -> Int
solve _  = 1

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day15: Beverage Bandits: Part1: solve -> (%d, %f)\n" result time
