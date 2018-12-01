{-|
Problem: <https://adventofcode.com/2018/day/1>

Solution:

General - Simple. Suming up numbers.

Part 1 - ???

Part 2 - ???
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
