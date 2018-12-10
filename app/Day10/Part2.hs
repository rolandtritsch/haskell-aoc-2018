module Day10.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day10

-- | solve the puzzle
solve :: Sky -> Int
solve _ = 2

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day10: Part2: The Stars Align: solve -> (%d, %f)\n" result time