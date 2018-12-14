module Day14.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day14

-- | solve the puzzle
solve :: String -> Int
solve target = gluehwein target' where
  target' = map (read . (:"")) target

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day14: Chocolate Charts: Part2: solve -> (%d, %f)\n" result time
