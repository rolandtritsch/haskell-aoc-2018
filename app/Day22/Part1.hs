module Day22.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day22

-- | solve the puzzle
solve :: (Depth, Position) -> Int
solve _ = 1

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day22: Mode Maze: Part1: solve -> (%d, %f)\n" result time
