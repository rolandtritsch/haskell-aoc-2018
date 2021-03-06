module Day13.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day13

-- | solve the puzzle.
solve :: (Grid, Carts) -> Position
solve (grid, carts) = go carts [] where
  go carts' [] = go carts'' collisions'' where
    (carts'', collisions'') = tick grid carts'
  go _ (p:_) = p

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day13: Mine Cart Madness: Part1: first -> (%s, %f)\n" (show result) time
