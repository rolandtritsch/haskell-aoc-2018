{- |
Problem: <https://adventofcode.com/2018/day/25>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day25 where

import Prelude hiding (Word)

import Text.Megaparsec (manyTill, eof, optional)
import Text.Megaparsec.Char (newline, string)

import Util (inputRaw, inputRaw1, inputParser, Parser, signedInteger)

type Point = (Int, Int, Int, Int)
type Points = [Point]

-- | read the input file
input :: [String]
input = inputRaw "input/Day25input.txt"

-- | read the input file (in one line)
input1 :: String
input1 = inputRaw1 "input/Day25input.txt"

-- | the parsed input.
parsedInput :: Points
parsedInput = inputParser parsePoints "input/Day25input.txt"

-- | parse the input.
parsePoints :: Parser Points
parsePoints = manyTill (parsePoint <* optional newline) eof

parsePoint :: Parser Point
parsePoint = (,,,)
  <$> signedInteger
  <* string ","
  <*> signedInteger
  <* string ","
  <*> signedInteger
  <* string ","
  <*> signedInteger
