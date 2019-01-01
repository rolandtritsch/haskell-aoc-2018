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

import Data.List (sortBy, delete)
import Data.Maybe (fromJust)
import Data.Ord (Down, comparing)

data AttackType
  = Bludgeoning
  | Cold
  | Fire
  | Radiation
  | Slashing
  deriving (Show, Eq)

data GType = WhiteBloodCells | Viruses deriving (Show, Eq)

data Group = Group {
  gType :: GType,
  gUnits :: Int,
  gHitpoints :: Int,
  gAttackDamage :: Int,
  gAttackType :: AttackType,
  gInitiativeLevel :: Int,
  gWeakTo :: [AttackType],
  gImmuneTo :: [AttackType]
  } deriving (Show, Eq)

type Army = [Group]
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
  <*> many (parseGroup WhiteBloodCells <* newline)
  <* newline
  <* string "Infection:" <* newline
  <*> many (parseGroup Viruses <* newline)
  <* eof

parseGroup :: GType -> Parser Group
parseGroup gt = toGroup
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
    toGroup units hitpoints (Just (weakTo, immuneTo)) attackPower attackType initiativeLevel = Group gt units hitpoints attackPower attackType initiativeLevel weakTo immuneTo
    toGroup units hitpoints Nothing attackPower attackType initiativeLevel = Group gt units hitpoints attackPower attackType initiativeLevel [] []

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

-- | calc the effective power of a group.
effectivePower :: Group -> Int
effectivePower g = (gUnits g) * (gAttackDamage g)

-- | sorting function (by increasing effective power and increasing initiative level)
byEffectivePower :: Group -> Group -> Ordering
byEffectivePower g g'
  | (effectivePower g) == (effectivePower g') = compare (gInitiativeLevel g) (gInitiativeLevel g')
  | otherwise = compare (effectivePower g) (effectivePower g')

-- | select a group to attack (white blood cells against viruses).
--
-- Algorithm goes like this ...
--
-- * sort by effectivePower (and initiativeLevel)
-- * reverse the sort (to get the groups in descending order)
-- * go over the groups and select a target for every group
--
selectTargets :: Armys -> [(Group, Maybe Group)]
selectTargets (wbcs, viruses) = go selectionOrder wbcs viruses where
  armys = wbcs ++ viruses
  selectionOrder = reverse $ sortBy byEffectivePower armys
  go (g':[]) wbcs' viruses'
    | (gType g') == WhiteBloodCells = [(g', fst $ (selectTarget g' viruses'))]
    | (gType g') == Viruses = [(g', fst $ (selectTarget g' wbcs'))]
  go (g':gs') wbcs' viruses'
    | (gType g') == WhiteBloodCells = (g', fst $ selectViruses) : go gs' wbcs' (snd $ selectViruses)
    | (gType g') == Viruses = (g', fst $ selectWbcs) : go gs' (snd $ selectWbcs) viruses'
    where
      selectViruses = selectTarget g' viruses'
      selectWbcs = selectTarget g' wbcs'
  go _ _ _ = error "selectTargets: go: Unexpected pattern match."

-- | select a/the target for a given group.
selectTarget :: Group -> [Group] -> (Maybe Group, [Group])
selectTarget _ [] = (Nothing, [])
selectTarget attacker enemies = (enemy', enemies') where
  possibleDamage = map (calcDamage' attacker) enemies
  calcDamage' a e = (calcDamage a e, e)
  highestDamage = fst $ head $ reverse $ sortBy (comparing fst) possibleDamage
  enemy'
    | highestDamage == 0 = Nothing
    | otherwise = Just $ head $ reverse $ sortBy byEffectivePower $ map snd $ filter ((== highestDamage) . fst) possibleDamage
  enemies'
    | highestDamage == 0 = enemies
    | otherwise = delete (fromJust enemy') enemies

-- | calculate the damage an attacker can do on an enemy.
calcDamage :: Group -> Group -> Int
calcDamage attacker enemy
   | elem (gAttackType attacker) (gImmuneTo enemy) = 0
   | elem (gAttackType attacker) (gWeakTo enemy) = (gAttackDamage attacker) * 2
   | otherwise = gAttackDamage attacker

-- | calculate the health of a group.
calcHealth :: Group -> Int
calcHealth g = (gUnits g) * (gHitpoints g)

-- | attack targets.
attackTargets :: Armys -> [(Group, Maybe Group)] -> Armys
attackTargets (wbcs, viruses) targets = go attackingOrder where
  armys = wbcs ++ viruses
  attackingOrder = sortBy (comparing (Down . gInitiativeLevel)) armys
  go _ = undefined

-- | attack a target and return the damaged target (or nothing, if the target was destroyed).
attackTarget :: Group -> Group -> Maybe Group
attackTarget attacker defender
  | calcDamage attacker defender >= calcHealth defender = Nothing
  | otherwise = Just $ defender { gUnits = remainingUnits }
  where
    remainingUnits = (gUnits defender) - killedUnits
    killedUnits = div (calcDamage attacker defender) (gHitpoints defender)
