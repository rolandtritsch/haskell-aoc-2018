{-|
Problem: <https://adventofcode.com/2018/day/2>

Solution:

General - Counting number of chars in a string.

Part 1 - Simple. Using Data.List.Unique (count) and look
for the right number of occurences of a given char.

Part 2 - Simple. Zip two boxIds and look for whats common.
-}
module Day02 where

import Util (inputRaw)

type BoxId = String

-- | read the input file
input :: [BoxId]
input = inputRaw "input/Day02input.txt"
