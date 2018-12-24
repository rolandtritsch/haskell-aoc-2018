module Day16.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day16

-- | solve the puzzle
solve :: Instructions -> Integer
solve _ = 2

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput2)
  printf "Day16: Template: Part2: solve -> (%d, %f)\n" result time
