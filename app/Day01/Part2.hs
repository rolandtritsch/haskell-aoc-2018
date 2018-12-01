module Day01.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day01

-- | solve the puzzle
solve :: [Frequency] -> Integer
solve fs = go [] $ scanl (+) 0 (cycle fs) where
  go seenFrequencySums (fsum:rest)
    | elem fsum seenFrequencySums = fsum
    | otherwise = go ([fsum] ++ seenFrequencySums) rest
  go _ [] = error "Infinite list cannot be empty."

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day01: Part2: solve -> (%f, %d)\n" time result
