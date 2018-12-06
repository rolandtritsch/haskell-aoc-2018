module Day06.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day06

-- | solve the puzzle
solve :: [Origin] -> Int
solve origins = snd $ largest $ buildGrid b origins where
  b = Boundary 0 500 0 500

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day06: Part1: Chronal Coordinates: largest -> (%d, %f)\n" result time
