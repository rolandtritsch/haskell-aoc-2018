module Day12.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day12

-- | solve the puzzle
solve :: (State, Notes) -> Int
solve (state, notes) = sumOfPotNumbers finalState where
  finalState = generations 20 notes state

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day12: Subterranean Sustainability: Part1: 20 -> (%d, %f)\n" result time
