{- |
Problem: <https://adventofcode.com/2018/day/1>

Solution:

General - Simple. Suming up numbers.

Part 1 - Sum up the list of numbers.

Part 2 - Turn the input into in indefinte stream of numbers/frequencies
(using cycle), sum up the sub-lists (with scanl) and (recursively) look
for the first duplicate to show up.
-}
module Day01 where

import Text.Megaparsec (manyTill, eof, optional)
import Text.Megaparsec.Char (newline)

import Util (inputRaw, inputRaw1, inputParser, Parser, signedInteger)

type Frequency = Int
type Frequencies = [Frequency]

-- | read the input file (handle the lines with a '+')
input :: Frequencies
input = (map processLine . inputRaw) "input/Day01input.txt" where
  processLine line@(sign:number)
    | sign == '+' = read number
    | otherwise = read line
  processLine [] = error "No input found."

-- | read the input file (in one line)
input1 :: String
input1 = inputRaw1 "input/Day01input.txt"

-- | the parsed input.
parsedInput :: Frequencies
parsedInput = inputParser parseFrequencies "input/Day01input.txt"

-- | parse the frequencies.
parseFrequencies :: Parser Frequencies
parseFrequencies = manyTill (parseFrequency <* optional newline) eof

parseFrequency :: Parser Frequency
parseFrequency = signedInteger
