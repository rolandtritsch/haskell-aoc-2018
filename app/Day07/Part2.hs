module Day07.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day07

-- | solve the puzzle
solve :: Int -> Int -> [Dependency] -> Int
solve delay ws ds = elapse where
  (elapse, _) = work ws (buildEffort delay) g (findRoots g) where
    g = buildGraph ds

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve 60 5 input)
  printf "Day07: The Sum of Its Parts: Part2: workers -> (%d, %f)\n" result time
