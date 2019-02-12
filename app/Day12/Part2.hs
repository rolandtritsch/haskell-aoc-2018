module Day12.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day12

-- | solve the puzzle
solve :: (State, Notes) -> Int
solve (state, notes) = gen + offset * constant where
  (gen, offset, constant) = detectShortcut notes 50000000000 state

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day12: Subterranean Sustainability: Part2: 50000000000 -> (%d, %f)\n" result time
