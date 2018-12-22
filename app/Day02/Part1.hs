module Day02.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Data.List.Unique (count)

import Day02

-- | solve the puzzle
solve :: BoxIds -> Int
solve boxIds = numberOfBoxesWithTwoLetters * numberOfBoxesWithThreeLetters where
  numberOfBoxesWithTwoLetters = (length . filter (numberOfLetters 2)) boxIds
  numberOfBoxesWithThreeLetters = (length . filter (numberOfLetters 3)) boxIds
  numberOfLetters n bid = any (\(_, c) -> c == n) $ count bid

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day02: Inventory Management System: Part1: numberOfLetters -> (%d, %f)\n" result time
