module Day25.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day25

-- | solve the puzzle
solve :: Points -> Int
solve _ = 2

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day25: Four-Dimensional Adventure: Part2: solve -> (%d, %f)\n" result time
