module Day24.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day24

-- | solve the puzzle
solve :: Groups -> Int
solve _ = 2

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day24: Immune System Simulator 20XX: Part2: solve -> (%d, %f)\n" result time
