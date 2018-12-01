{-|
Problem: <https://adventofcode.com/2018/day/1>

Solution:

General - Simple. Suming up numbers.

Part 1 - Sum up the list of numbers.

Part 2 - Turn the input into in indefinte stream of numbers/frequencies
(using cycle), sum up the sub-lists (with scanl) and (recursively) look
for the first duplicate to show up.
-}
module Day01 where

import Util (inputRaw)

type Frequency = Integer

-- | read the input file (handle the lines with a '+')
input :: [Frequency]
input = (map processLine . inputRaw) "input/Day01input.txt" where
  processLine line@(sign:number)
    | sign == '+' = read number
    | otherwise = read line
  processLine [] = error "No input found."
