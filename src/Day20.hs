{-|
Problem: <https://adventofcode.com/2018/day/20>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day20 where

import Text.Megaparsec (many, manyTill, (<|>))
import Text.Megaparsec.Char (char)

import Util (inputRaw, inputRaw1, inputParser, Parser)

data Direction = North | South | West | East deriving (Show, Eq)
data Route = Path [Direction] | Branch Routes Routes deriving (Show, Eq)
type Routes = [Route]

-- | read the input file
input :: [String]
input = inputRaw "input/Day20input.txt"

-- | read the input file (as one line)
input1 :: String
input1 = inputRaw1 "input/Day20input.txt"

-- | the parsed input.
parsedInput :: Routes
parsedInput = inputParser parseRoutes "input/Day20input.txt"

-- | parse the regex and turn it into a sequence/list of Paths and Branches.
parseRoutes :: Parser Routes
parseRoutes = parseBeginOfRegex *> manyTill parseRoute parseEndOfRegex

parseRoute :: Parser Route
parseRoute = parseBranch <|> parsePath

parsePath :: Parser Route
parsePath = Path <$> many parseDirection

parseBranch :: Parser Route
parseBranch = Branch
  <$ parseBeginOfBranch
  <*> manyTill parseRoute parseBranchSeperator
  <*> manyTill parseRoute parseEndOfBranch

parseBeginOfRegex, parseEndOfRegex :: Parser Char
parseBeginOfRegex = char '^'
parseEndOfRegex = char '$'

parseDirection :: Parser Direction
parseDirection = parseNorth <|> parseSouth <|> parseWest <|> parseEast

parseNorth, parseSouth, parseWest, parseEast :: Parser Direction
parseNorth = North <$ char 'N'
parseSouth = South <$ char 'S'
parseWest = West <$ char 'W'
parseEast = East <$ char 'E'

parseBeginOfBranch, parseBranchSeperator, parseEndOfBranch :: Parser Char
parseBeginOfBranch = char '('
parseEndOfBranch = char ')'
parseBranchSeperator = char '|'
