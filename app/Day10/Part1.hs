module Day10.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day10

-- | solve the puzzle
solve :: Sky -> [String]
solve sky = paint message where
  (_, message) = night 0 sky

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day10: Part1: The Stars Align: message -> (%s, %f)\n" (show result) time
