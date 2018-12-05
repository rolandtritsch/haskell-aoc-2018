module Day05.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Data.Ord (comparing)
import Data.List (minimumBy)

import Day05

-- | solve the puzzle
solve :: String -> Int
solve p = snd $ minimumBy (comparing snd) allReactions where
  allReactions = [(t, reactionLength t) | t <- ['a'..'z']] where
    reactionLength t = (length . reaction) (filter (\(Unit t' _) -> t /= t') p') where
      p' = buildPolymer p

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day05: Part2: Alchemical Reduction: minLength -> (%d, %f)\n" result time
