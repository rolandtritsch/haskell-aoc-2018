module Day08.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day08

-- | solve the puzzle
solve :: [Int] -> Int
solve inputStream = (sum . fmap sum) $ createRootNode inputStream

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day08: Memory Maneuver: Part1: sum -> (%d, %f)\n" result time
