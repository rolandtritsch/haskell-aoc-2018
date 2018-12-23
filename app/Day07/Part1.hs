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
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day07: The Sum of Its Parts: Part1: path -> (%s, %f)\n" result time
