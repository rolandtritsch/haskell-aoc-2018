module Day15.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day15

-- | solve the puzzle
solve :: BattleGround -> Integer
solve _ = 2

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day15: Beverage Bandits: Part2: solve -> (%d, %f)\n" result time
