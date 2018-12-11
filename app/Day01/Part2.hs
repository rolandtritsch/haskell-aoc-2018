module Day01.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import qualified Data.Set as S

import Day01

-- | solve the puzzle
solve :: [Frequency] -> Integer
solve fs = go S.empty $ scanl (+) 0 (cycle fs) where
  go seenFrequencySums (fsum:rest)
    | S.member fsum seenFrequencySums = fsum
    | otherwise = go (S.insert fsum seenFrequencySums) rest
  go _ [] = error "Infinite list cannot be empty."

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day01: Chronal Calibration: Part2: seenFrequencySums -> (%d, %f)\n" result time
