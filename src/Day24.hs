{-|
Problem: <https://adventofcode.com/2018/day/24>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day24 where

import Text.Megaparsec (many, eof, optional, (<|>))
import Text.Megaparsec.Char (newline, string)

import Util (inputRaw, inputRaw1, inputParser, Parser, integer)

data AttackType
  = Bludgeoning
  | Cold
  | Fire
  | Radiation
  | Slashing
  deriving (Show, Eq)
data SoldierGroup = SoldierGroup {
  sgUnits :: Int,
  sgHitpoints :: Int,
  sgAttackDamage :: Int,
  sgAttackType :: AttackType,
  sgInitiativeLevel :: Int,
  sgWeakTo :: [AttackType],
  sgImmuneTo :: [AttackType]
  } deriving (Show, Eq)
type Army = [SoldierGroup]
type Armys = (Army, Army)

-- | read the input file
input :: [String]
input = inputRaw "input/Day24input.txt"

-- | read the input file (as one line)
input1 :: String
input1 = inputRaw1 "input/Day24input.txt"

-- | the parsed input.
parsedInput :: Armys
parsedInput = inputParser parseArmys "input/Day24input.txt"

-- | parse the regex and turn it into a sequence/list of Paths and Branches.
parseArmys :: Parser Armys
parseArmys = (,)
  <$ string "Immune System:" <* newline
  <*> many (parseSoldierGroup <* newline)
  <* newline
  <* string "Infection:" <* newline
  <*> many (parseSoldierGroup <* newline)
  <* eof

parseSoldierGroup :: Parser SoldierGroup
parseSoldierGroup = toSoliderGroup
  <$> integer
  <* string " units each with "
  <*> integer
  <* string " hit points "
  <*> optional (parseQualities <* string " ")
  <* string "with an attack that does "
  <*> integer
  <* string " "
  <*> parseAttackType
  <* string " damage at initiative "
  <*> integer
  where
    toSoliderGroup units hitpoints (Just (weakTo, immuneTo)) attackPower attackType initiativeLevel = SoldierGroup units hitpoints attackPower attackType initiativeLevel weakTo immuneTo
    toSoliderGroup units hitpoints Nothing attackPower attackType initiativeLevel = SoldierGroup units hitpoints attackPower attackType initiativeLevel [] []

parseQualities :: Parser ([AttackType], [AttackType])
parseQualities = (,)
  <$ string "("
  *> (parseWB4I <|> parseIB4W)
  <* string ")"

parseWB4I :: Parser ([AttackType], [AttackType])
parseWB4I = toWB4I <$> parseWeaknesses <*> optional (string "; " *> parseImmunities) where
  toWB4I w (Just i) = (w, i)
  toWB4I w Nothing = (w, [])

parseIB4W :: Parser ([AttackType], [AttackType])
parseIB4W = toIB4W <$> parseImmunities <*> optional (string "; " *> parseWeaknesses) where
  toIB4W i (Just w) = (w, i)
  toIB4W i Nothing = ([], i)

parseImmunities :: Parser [AttackType]
parseImmunities = string "immune to " *> (many (parseAttackType <* optional (string ", ")))

parseWeaknesses :: Parser [AttackType]
parseWeaknesses = string "weak to " *> (many (parseAttackType <* optional (string ", ")))

parseAttackType :: Parser AttackType
parseAttackType = parseBludgeoning <|> parseCold <|> parseFire <|> parseRadiation <|> parseSlashing

parseBludgeoning, parseCold, parseFire, parseRadiation, parseSlashing :: Parser AttackType
parseBludgeoning = Bludgeoning <$ string "bludgeoning"
parseCold = Cold <$ string "cold"
parseFire = Fire <$ string "fire"
parseRadiation = Radiation <$ string "radiation"
parseSlashing = Slashing <$ string "slashing"
