module Day20.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day20

-- | solve the puzzle
solve :: Routes -> Integer
solve _ = 2

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day20: A Regular Map: Part2: solve -> (%d, %f)\n" result time
