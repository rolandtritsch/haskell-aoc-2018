module Day18.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day18

-- | solve the puzzle
solve :: Acres -> Integer
solve _ = 1

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day18: Settlers of The North Pole: Part1: solve -> (%d, %f)\n" result time
