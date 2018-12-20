module Day17.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day17

-- | solve the puzzle
solve :: [String] -> Integer
solve _ = 2

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day17: Reservoir Research: Part2: solve -> (%d, %f)\n" result time
