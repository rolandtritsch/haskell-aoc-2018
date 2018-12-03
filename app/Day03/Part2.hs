module Day03.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day03

-- | solve the puzzle
solve :: [Claim] -> Integer
solve _ = 2

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day03: Part2: solve -> (%d, %f)\n" result time
