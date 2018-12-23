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
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day06: Chronal Coordinates: Part1: largest -> (%d, %f)\n" result time
