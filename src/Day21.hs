{-|
Problem: <https://adventofcode.com/2018/day/21>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day21 where

import Util (inputRaw, inputRaw1, inputParser)

import Day19 (InstructionPointer, Instructions, parseProgram)

-- | read the input file
input :: [String]
input = inputRaw "input/Day21input.txt"

-- | read the input file (as one line)
input1 :: String
input1 = inputRaw1 "input/Day21input.txt"

-- | the parsed input.
parsedInput :: (InstructionPointer, Instructions)
parsedInput = inputParser parseProgram "input/Day21input.txt"
