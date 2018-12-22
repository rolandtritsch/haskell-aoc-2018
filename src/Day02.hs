{-|
Problem: <https://adventofcode.com/2018/day/2>

Solution:

General - Counting number of chars in a string.

Part 1 - Simple. Using Data.List.Unique (count) and look
for the right number of occurences of a given char.

Part 2 - Simple. Zip two boxIds and look for whats common.
-}
module Day02 where

import Text.Megaparsec (manyTill, eof, optional, many)
import Text.Megaparsec.Char (newline, lowerChar)

import Util (inputRaw, inputRaw1, inputParser, Parser)

type BoxId = String
type BoxIds = [BoxId]

-- | read the input file
input :: BoxIds
input = inputRaw "input/Day02input.txt"

-- | read the input file
input1 :: String
input1 = inputRaw1 "input/Day02input.txt"

-- | the parsed input.
parsedInput :: BoxIds
parsedInput = inputParser parseBoxIds "input/Day02input.txt"

parseBoxIds :: Parser BoxIds
parseBoxIds = manyTill (parseBoxId <* optional newline) eof

parseBoxId :: Parser BoxId
parseBoxId = many lowerChar
