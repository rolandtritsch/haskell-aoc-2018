module Day07.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day07

-- | solve the puzzle
solve :: [Dependency] -> [Step]
solve _ = "2"

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day07: Part2: The Sum of Its Parts: workers -> (%s, %f)\n" result time
