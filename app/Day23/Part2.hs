module Day23.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day23

-- | solve the puzzle
solve :: NanoBots -> Int
solve _ = 2

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day23: Experimental Emergency Teleportation: Part2: solve -> (%d, %f)\n" result time
