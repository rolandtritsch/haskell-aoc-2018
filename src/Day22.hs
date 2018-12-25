{-|
Problem: <https://adventofcode.com/2018/day/22>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day22 where

import Text.Megaparsec (eof)
import Text.Megaparsec.Char (string, newline)

import Util (inputRaw, inputParser, Parser, integer)

type Depth = Int
type Position = (Int, Int)

-- | read the input file
input :: [String]
input = inputRaw "input/Day22input.txt"

-- | the parsed input.
parsedInput :: (Depth, Position)
parsedInput = inputParser parseInput "input/Day22input.txt"

-- | parse the input.
parseInput  :: Parser (Depth, Position)
parseInput = (,) <$> parseDepth <* newline <*> parseTarget <* newline <* eof

parseDepth :: Parser Int
parseDepth = string "depth: " *> integer

parseTarget :: Parser Position
parseTarget = (,) <$ string "target: " <*> integer <* string "," <*> integer
