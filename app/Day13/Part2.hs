module Day13.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day13

-- | solve the puzzle
solve :: [String] -> Integer
solve _ = 2

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day13: Template: Part2: solve -> (%d, %f)\n" result time
