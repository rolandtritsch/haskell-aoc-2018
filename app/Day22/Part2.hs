module Day22.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day22

-- | solve the puzzle
solve :: (Depth, Position) -> Int
solve _ = 2

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day22: Template: Part2: solve -> (%d, %f)\n" result time
