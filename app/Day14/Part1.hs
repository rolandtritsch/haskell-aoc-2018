module Day14.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day14

-- | solve the puzzle
solve :: String -> String
solve target = hint target' (chocolate target') where
  target' = read target

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day14: Chocolate Charts: Part1: hint -> (%s, %f)\n" result time
