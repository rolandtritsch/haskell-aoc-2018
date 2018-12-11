module Day05.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day05

-- | solve the puzzle
solve :: String -> Int
solve p = (length . reaction) (buildPolymer p)

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day05: Alchemical Reduction: Part1: length -> (%d, %f)\n" result time
