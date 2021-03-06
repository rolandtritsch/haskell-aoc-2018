module Day16.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day16

-- | solve the puzzle
solve :: Observations -> Integer
solve _ = 1

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput1)
  printf "Day16: Template: Part1: solve -> (%d, %f)\n" result time
