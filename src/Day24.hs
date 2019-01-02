{-|
Problem: <https://adventofcode.com/2018/day/24>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day24 where

import Debug.Trace

import Text.Megaparsec (many, eof, optional, (<|>))
import Text.Megaparsec.Char (newline, string)

import Util (inputRaw, inputRaw1, inputParser, Parser, integer)

import Data.List (sortBy)
import Data.Maybe (fromJust)
import Data.Ord

import qualified Data.Map as M

data AttackType
  = Bludgeoning
  | Cold
  | Fire
  | Radiation
  | Slashing
  deriving (Show, Eq, Ord)

data GType = WhiteBloodCells | Viruses deriving (Show, Eq, Ord)

data Group = Group {
  gType :: GType,
  gUnits :: Int,
  gHitpoints :: Int,
  gAttackDamage :: Int,
  gAttackType :: AttackType,
  gInitiativeLevel :: Int,
  gWeakTo :: [AttackType],
  gImmuneTo :: [AttackType]
  } deriving (Show, Eq, Ord)

type GId = Int
type Groups = M.Map GId Group
type Targets = M.Map GId GId

-- | read the input file
input :: [String]
input = inputRaw "input/Day24input.txt"

-- | read the input file (as one line)
input1 :: String
input1 = inputRaw1 "input/Day24input.txt"

-- | the parsed input.
parsedInput :: Groups
parsedInput = M.fromList $ zip [0..] $ inputParser parseGroups "input/Day24input.txt"

-- | parse the regex and turn it into a sequence/list of Paths and Branches.
parseGroups :: Parser [Group]
parseGroups = (++)
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
effectivePower :: GId -> Groups -> Int
effectivePower gId gs = (gUnits (gs M.! gId)) * (gAttackDamage (gs M.! gId))

effectivePower' :: Group -> Int
effectivePower' g = (gUnits g) * (gAttackDamage g)

-- | sorting function (by increasing effective power and increasing initiative level)
byEffectivePower :: Group -> Group -> Ordering
byEffectivePower g g'
  | (effectivePower' g) == (effectivePower' g') = compare (gInitiativeLevel g) (gInitiativeLevel g')
  | otherwise = compare (effectivePower' g) (effectivePower' g')

-- | calculate the damage an attacker can do on an enemy.
calcDamage :: Group -> Group -> Int
calcDamage attacker enemy
   | elem (gAttackType attacker) (gImmuneTo enemy) = 0
   | elem (gAttackType attacker) (gWeakTo enemy) = (gAttackDamage attacker) * 2
   | otherwise = gAttackDamage attacker

-- | select a group to attack (white blood cells against viruses).
--
-- Algorithm goes like this ...
--
-- * sort by effectivePower (and initiativeLevel)
-- * reverse the sort (to get the groups in descending order)
-- * go over the groups and select a target for every group
--
selectTargets :: Groups -> Targets
selectTargets groups = go selectionOrder wbcs viruses M.empty where
  wbcs = M.filter ((== WhiteBloodCells) . gType) groups
  viruses = M.filter ((== Viruses) . gType) groups
  selectionOrder = reverse $ map fst $ sortBy byEffectivePower' (M.toList groups) where
    byEffectivePower' (_, g) (_, g') = byEffectivePower g g'
  insertGId gId' (Just eId') targets' = M.insert gId' eId' targets'
  insertGId _ Nothing targets' = targets'
  go (gId':[]) wbcs' viruses' targets'
    | (gType g') == WhiteBloodCells = insertGId gId' selectedVirusGId targets'
    | (gType g') == Viruses = insertGId gId' selectedWbcsGId targets'
    where
      g' = groups M.! gId'
      (selectedVirusGId, _) = selectTarget g' viruses'
      (selectedWbcsGId, _) = selectTarget g' wbcs'
  go (gId':gIds') wbcs' viruses' targets'
    | (gType g') == WhiteBloodCells = go gIds' wbcs' remainingViruses (insertGId gId' selectedVirusGId targets')
    | (gType g') == Viruses = go gIds' remainingWbcs viruses' (insertGId gId' selectedWbcsGId targets')
    where
      g' = groups M.! gId'
      (selectedVirusGId, remainingViruses) = selectTarget g' viruses'
      (selectedWbcsGId, remainingWbcs) = selectTarget g' wbcs'
  go _ _ _ _ = error "selectTargets: go: Unexpected pattern match."

-- | select a/the target for a given group from the map of availableEnemies.
selectTarget :: Group -> Groups -> (Maybe GId, Groups)
selectTarget attacker availableEnemies
  | M.null availableEnemies = (Nothing, M.empty)
  | otherwise = (enemyId', remainingEnemies')
  where
    possibleDamage = map (calcDamage' attacker) (M.toList availableEnemies)
    calcDamage' a (eId, e) = (calcDamage a e , eId, e)
    (highestDamage, _, _) = traceShow possibleDamage $ head $ sortBy (comparing (Down . byDamage)) possibleDamage where
      byDamage (d, _, _) = d
    enemyId'
      | highestDamage == 0 = Nothing
      | otherwise = traceShow highestDamage $ Just $ snd' $ head $ reverse $ sortBy byEffectivePower' $ filter ((== highestDamage) . fst') possibleDamage
      where
        fst' (d, _, _) = d
        snd' (_, eId, _) = eId
        byEffectivePower' (_, _, g) (_, _, g') = byEffectivePower g g'
    remainingEnemies'
      | highestDamage == 0 = availableEnemies
      | otherwise = M.delete (fromJust enemyId') availableEnemies

-- | calculate the health of a group.
calcHealth :: Group -> Int
calcHealth g = (gUnits g) * (gHitpoints g)

-- | fight the fight (one round) and return the remaining groups (with a/the
-- updated unit count).
--
-- Algorithm goes like this ...
--
-- * get the right attacking order.
-- * for every attacker ...
--   - check if it has a target and if so attack the target
--     - if the target is eliminated, remove it from the remainingAttackers (if it is
--       in the list of remainingAttackers)
--     - otherwise add it (means, what is left over) to the map of groups that will fight
--       the next fight
--   - otherwise add it to the map of groups that will fight the next fight (if it not
--     there already)
--
fight :: Groups -> Targets -> Groups
fight groups targets = go attackingOrder M.empty where
  attackingOrder = map fst $ sortBy (comparing (Down . gInitiativeLevel . snd)) (M.toList groups)
  insertGIdIfNotThere gId g gs
    | M.member gId gs = gs
    | otherwise = M.insert gId g gs
  insertGIdIfNotNothing gId (Just g) gs = M.insert gId g gs
  insertGIdIfNotNothing gId Nothing gs = M.delete gId gs
  go (attackerId:[]) nextGroups
    | M.member attackerId targets = nextGroups'
    | otherwise = nextGroups''
    where
      defenderId = targets M.! attackerId
      attackedGroup = attack (groups M.! attackerId) (groups M.! defenderId)
      nextGroups' = insertGIdIfNotThere attackerId (groups M.! attackerId) $ insertGIdIfNotNothing defenderId attackedGroup nextGroups
      nextGroups'' = insertGIdIfNotThere attackerId (groups M.! attackerId) nextGroups
  go (attackerId:remainingAttackerIds) nextGroups
    | M.member attackerId targets = go remainingAttackerIds nextGroups'
    | otherwise = go remainingAttackerIds nextGroups''
    where
      defenderId = targets M.! attackerId
      attackedGroup = attack (groups M.! attackerId) (groups M.! defenderId)
      nextGroups' = insertGIdIfNotThere attackerId (groups M.! attackerId) $ insertGIdIfNotNothing defenderId attackedGroup nextGroups
      nextGroups'' = insertGIdIfNotThere attackerId (groups M.! attackerId) nextGroups
  go _ _ = error "fight: go: Unexpected pattern match."

-- | attack a target and return the damaged target (or nothing, if the target was destroyed).
attack :: Group -> Group -> Maybe Group
attack attacker defender
  | calcDamage attacker defender >= calcHealth defender = Nothing
  | otherwise = Just $ defender { gUnits = remainingUnits }
  where
    remainingUnits = (gUnits defender) - killedUnits
    killedUnits = div (calcDamage attacker defender) (gHitpoints defender)

-- | compat it out (between the white bloodcells and the viruses) until one side
-- has no groups left over.
combat :: Groups -> Groups
combat groups
  | doneFighting = groups
  | otherwise = combat $ fight groups $ selectTargets groups
  where
    doneFighting = (M.size wbcs == 0) || (M.size viruses == 0) where
      wbcs = M.filter ((== WhiteBloodCells) . gType) groups
      viruses = M.filter ((== Viruses) . gType) groups
