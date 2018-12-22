module Day05.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Data.List (nub)

import Day05

-- | solve the puzzle
solve :: Polymer -> Int
solve p = minimum allReactionLengths where
  firstReaction = reaction p
  unitTypes = nub $ map (\(Unit t _) -> t) firstReaction
  allReactionLengths = [reactionLength t | t <- unitTypes] where
    reactionLength t = (length . reaction) (filter (\(Unit t' _) -> t /= t') firstReaction)

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day05: Alchemical Reduction: Part2: minLength -> (%d, %f)\n" result time
