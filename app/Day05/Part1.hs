module Day05.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day05

-- | solve the puzzle
solve :: Polymer -> Int
solve p = (length . reaction) p

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day05: Alchemical Reduction: Part1: length -> (%d, %f)\n" result time
