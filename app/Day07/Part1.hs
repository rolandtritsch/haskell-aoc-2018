module Day07.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day07

-- | solve the puzzle
solve :: [Dependency] -> [Step]
solve ds = findPath g (findRoots g) where
  g = buildGraph ds

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day07: Part1: The Sum of Its Parts: path -> (%s, %f)\n" result time
