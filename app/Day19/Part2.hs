module Day19.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day19

-- | solve the puzzle
solve :: (InstructionPointer, Instructions) -> Integer
solve _ = 2

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day19: Go With The Flow: Part2: solve -> (%d, %f)\n" result time
