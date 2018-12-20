{-|
Problem: <https://adventofcode.com/2018/day/17>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day17 where

import Text.Megaparsec (manyTill, eof, (<|>), optional)
import Text.Megaparsec.Char (string, newline)
import Text.Megaparsec.Char.Lexer (decimal)

import qualified Data.Map as M

import Util (inputRaw, inputRaw1, Parser)

type Position = (Int, Int)
data Vein = Vein Position Position deriving (Show, Eq)
data Ground = Sand | Clay deriving (Show, Eq)
data Water = Flowing | Settled deriving (Show, Eq)

type Underground = M.Map Position Ground
type Waterfall = M.Map Position Water

-- | read the input file
input :: [String]
input = inputRaw "input/Day17input.txt"

-- | read the input file
input1 :: String
input1 = inputRaw1 "input/Day17input.txt"

-- | parse a horizontal vein.
parseHorizontalVein :: Parser Vein
parseHorizontalVein = toVein
  <$ string "x="
  <*> decimal
  <* string ", "
  <* string "y="
  <*> decimal
  <* string ".."
  <*> decimal
  <* newline
  where
    toVein x y y' = Vein (x,y) (x,y')

-- | parse a vertical vein.
parseVerticalVein :: Parser Vein
parseVerticalVein = toVein
  <$ string "y="
  <*> decimal
  <* string ", "
  <* string "x="
  <*> decimal
  <* string ".."
  <*> decimal
  where
    toVein y x x' = Vein (x,y) (x',y)

-- | parse all the veins.
parseVeins :: Parser [Vein]
parseVeins = manyTill (parseHorizontalVein <|> parseVerticalVein <* optional newline) eof
