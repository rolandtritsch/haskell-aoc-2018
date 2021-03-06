module Day20.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day20

-- | solve the puzzle
solve :: Routes -> Integer
solve _ = 1

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day20: A Regular Map: Part1: solve -> (%d, %f)\n" result time
