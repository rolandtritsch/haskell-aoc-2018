module Day23.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day23

-- | solve the puzzle
solve :: NanoBots -> Int
solve _ = 1

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day23: Experimental Emergency Teleportation: Part1: solve -> (%d, %f)\n" result time
