{-|
Problem: <https://adventofcode.com/2018/day/18>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day18 where

import Text.Megaparsec ((<|>), manyTill, eof, optional)
import Text.Megaparsec.Char (char, newline)

import Util (inputRaw, inputRaw1, Parser)

data Acre = Ground | Trees | Yard deriving (Show, Eq)
type Acres = [Acre]

-- | read the input file
input :: [String]
input = inputRaw "input/Day18input.txt"

-- | read the input file (in one line)
input1 :: String
input1 = inputRaw1 "input/Day18input.txt"

-- | parse an acre.
parseAcre, parseGround, parseTrees, parseYard :: Parser Acre
parseAcre = parseGround <|> parseTrees <|> parseYard
parseGround = Ground <$ char '.'
parseTrees = Trees <$ char '|'
parseYard =  Yard <$ char '#'

-- | parse the area.
parseArea :: Parser Acres
parseArea = manyTill (parseAcre <* optional newline) eof
