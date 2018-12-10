module Day10.Part1 where

import Debug.Trace

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day10

-- | solve the puzzle
solve :: Sky -> Int
solve sky = traceShow message $ secs where
  (secs, message) = night 0 sky

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day10: Part1: The Stars Align: solve -> (%d, %f)\n" result time
