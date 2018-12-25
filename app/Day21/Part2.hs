module Day21.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day19 (InstructionPointer, Instructions)

import Day21

-- | solve the puzzle
solve :: (InstructionPointer, Instructions) -> Int
solve _ = 2

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day21: Chronal Conversion: Part2: solve -> (%d, %f)\n" result time
