{-|
Problem: <https://adventofcode.com/2018/day/23>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day23 where

import Text.Megaparsec (manyTill, eof)
import Text.Megaparsec.Char (newline, string)

import Util (inputRaw, inputRaw1, inputParser, Parser, signedInteger, integer)

type Position = (Int, Int, Int)
type Radius = Int
data NanoBot = NanoBot Position Radius deriving (Show, Eq)
type NanoBots = [NanoBot]

-- | read the input file
input :: [String]
input = inputRaw "input/Day23input.txt"

-- | read the input file (as one line)
input1 :: String
input1 = inputRaw1 "input/Day23input.txt"

-- | the parsed input.
parsedInput :: NanoBots
parsedInput = inputParser parseNanoBots "input/Day23input.txt"

-- | parse the regex and turn it into a sequence/list of Paths and Branches.
parseNanoBots :: Parser NanoBots
parseNanoBots = manyTill (parseNanoBot <* newline) eof

parseNanoBot :: Parser NanoBot
parseNanoBot = NanoBot
  <$> parsePosition
  <* string ", "
  <*> parseRadius

parsePosition :: Parser Position
parsePosition = (,,)
  <$ string "pos=<"
  <*> signedInteger
  <* string ","
  <*> signedInteger
  <* string ","
  <*> signedInteger
  <* string ">"

parseRadius :: Parser Int
parseRadius = string "r=" *> integer
