{-|
Problem: <https://adventofcode.com/2018/day/0>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day00 where

import Prelude hiding (Word)

import Text.Megaparsec (manyTill, many, eof, optional)
import Text.Megaparsec.Char (newline, alphaNumChar)

import Util (inputRaw, inputRaw1, Parser)

type Word = String
type Words = [Word]

-- | read the input file
input :: [String]
input = inputRaw "input/Day00input.txt"

-- | read the input file (in one line)
input1 :: String
input1 = inputRaw1 "input/Day00input.txt"

-- | parse the input
parseWords :: Parser Words
parseWords = manyTill (parseWord <* optional newline) eof

parseWord :: Parser Word
parseWord = many alphaNumChar
