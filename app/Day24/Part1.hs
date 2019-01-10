module Day24.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import qualified Data.Map as M

import Day24

-- | solve the puzzle
solve :: Groups -> Int
solve gs = remainingUnits where
  remainingUnits = sum $ map gUnits $ M.elems lastManStanding
  lastManStanding = combat gs

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day24: Immune System Simulator 20XX: Part1: solve -> (%d, %f)\n" result time
