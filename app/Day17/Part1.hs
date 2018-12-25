module Day17.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day17

-- | solve the puzzle
solve :: Veins -> Integer
solve _ = 1

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day17: Reservoir Research: Part1: solve -> (%d, %f)\n" result time
