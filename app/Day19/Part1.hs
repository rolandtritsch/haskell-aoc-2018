module Day19.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day19

-- | solve the puzzle
solve :: (InstructionPointer, Instructions) -> Integer
solve _ = 1

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day19: Go With The Flow: Part1: solve -> (%d, %f)\n" result time
