{-|
Problem: <https://adventofcode.com/2018/day/5>

Solution:

General - Simple. Lots of (good :)) pattern matching
and lost of (good :)) recursions.

The main idea is build the polymer as a list of units
and then recurse through the list to do a reaction.

We then do another recursion to do all reactions until
there are no more to do.

Part 1 - Do the reaction and return the length of the
resulting polymer.

Part 2 - Do all reactions (with one unit type missing)
and return the minimum length.
-}
module Day05 where

import Text.Megaparsec (many, eof, (<|>))
import Text.Megaparsec.Char (upperChar, lowerChar, newline)

import Util (inputRaw, inputRaw1, inputParser, Parser)

import Data.Char

type Type = Char
data Polarity = Plus | Minus deriving (Show, Eq)
data Unit = Unit Type Polarity deriving (Show, Eq)

type Polymer = [Unit]

-- | read the input file
input :: Polymer
input = (buildPolymer . head . inputRaw) "input/Day05input.txt" where

-- | build a/the polymer.
buildPolymer :: String -> Polymer
buildPolymer = map buildPolymer' where
  buildPolymer' c = Unit (toLower c) (p (isLower c)) where
    p True = Minus
    p False = Plus

-- | read the input file
input1 :: String
input1 = inputRaw1 "input/Day05input.txt"

-- | the parsed input.
parsedInput :: Polymer
parsedInput = inputParser parsePolymer "input/Day05input.txt"

parsePolymer :: Parser Polymer
parsePolymer = many (parsePositiveUnit <|> parseNegativeUnit) <* newline <* eof

parsePositiveUnit, parseNegativeUnit :: Parser Unit
parsePositiveUnit = toUnit <$> upperChar where
  toUnit c = Unit (toLower c) Plus
parseNegativeUnit = toUnit <$> lowerChar where
  toUnit c = Unit c Minus

-- | can these two Units react?
canReact :: Unit -> Unit -> Bool
canReact (Unit t p) (Unit t' p') = t == t' && p /= p'

-- | take a Polymer, find the first 2 Units that can react, react and
-- return the resulting Polymer.
react :: Polymer -> Polymer
react = foldr bang [] where
  bang u (u':us')
    | canReact u u' = us'
    | otherwise = u:u':us'
  bang u [] = [u]

react'' :: Polymer -> Polymer
react'' = foldl bang [] where
  bang [] u = [u]
  bang newP u
    | canReact (last newP) u = init newP
    | otherwise = newP ++ [u]

react' :: Polymer -> Polymer
react' (u:[]) = [u]
react' (u:u':[])
  | canReact u u' = []
  | otherwise = [u] ++ [u']
react' (u:rest@(u':rest'))
  | canReact u u' = react' rest'
  | otherwise = [u] ++ react' rest
react' p = error ("Unexpected pattern match: " ++ show p)

-- | react until there is nothing to react on.
reaction :: Polymer -> Polymer
reaction p
  | p == nextP = p
  | otherwise = reaction nextP
  where
    nextP = react p
