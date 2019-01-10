{-|
Problem: <https://adventofcode.com/2018/day/24>

Solution:

General - Wow. Biggest challenge for this one: To read and understand
the problem statement and to turn it into code.

Main idea(s) are ...

* I will call the groups that make up the immune system the "white blood
cells" and the groups that make up the infection the "viruses".

* read all of the groups. Initially I had two seperate lists of groups,
but some of the decisions need to be made regardless of the group type,
means it is easier to put the group type into the group.

* then I realized that in the target selection phase and in the attack
phase there are corner cases that require me to uniquely identify which
group I am reading/updating/deleting. Means I need a primary key. The
primary key are all fields besides the number of units (the only field
that actually changes during the combat). Was not sure, if it is a good
idea to have such a large primary key and decided to create an artificial
primary key (the group id (gId)).

* means most of the processing happens by processing gIds

Part 1 - ???

Part 2 - ???
-}
module Day24 where

import Text.Megaparsec (many, eof, optional, (<|>))
import Text.Megaparsec.Char (newline, string)

import Util (inputRaw, inputRaw1, inputParser, Parser, integer)

import Data.List (sortBy, delete)
import Data.Maybe (fromJust, isNothing)
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
  } deriving (Show, Eq)

-- | order groups by decreasing effective power and decreasing initiative level
instance Ord Group where
  compare g g'
    | (effectivePower' g) == (effectivePower' g') = comparing (Down . gInitiativeLevel) g g'
    | otherwise = comparing (Down . effectivePower') g g'

type GId = Int
type Groups = M.Map GId Group
type Targets = M.Map GId GId

-- | read the input file
input :: [String]
input = inputRaw "input/Day24input.txt"

-- | read the input file (as one line)
input1 :: String
input1 = inputRaw1 "input/Day24input.txt"

-- | the parsed input (building a/the map of GId to Group).
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

---------------------------------------------------------------------------------

-- | calc the effective power of a group.
effectivePower :: GId -> Groups -> Int
effectivePower gId gs = (gUnits (gs M.! gId)) * (gAttackDamage (gs M.! gId))

effectivePower' :: Group -> Int
effectivePower' g = (gUnits g) * (gAttackDamage g)

-- | calculate the damage an attacker can do to a defender.
calcDamage :: GId -> GId -> Groups -> Int
calcDamage attackerId defenderId groups
   | elem (gAttackType attacker) (gImmuneTo defender) = 0
   | elem (gAttackType attacker) (gWeakTo defender) = (effectivePower attackerId groups) * 2
   | otherwise = effectivePower' attacker
   where
     attacker = groups M.! attackerId
     defender = groups M.! defenderId

calcDamage' :: Group -> Group -> Int
calcDamage' attacker defender
   | elem (gAttackType attacker) (gImmuneTo defender) = 0
   | elem (gAttackType attacker) (gWeakTo defender) = (effectivePower' attacker) * 2
   | otherwise = effectivePower' attacker

-- | select a group to attack (white blood cells against viruses).
--
-- Algorithm goes like this ...
--
-- * get (sorted) list of target ids and go over the ids
-- * for every id select a target (if there is one)
--
selectTargets :: Groups -> Targets
selectTargets groups = snd $ foldl go ((wbcs, viruses), M.empty) selectionOrder where
  wbcs = M.filter ((== WhiteBloodCells) . gType) groups
  viruses = M.filter ((== Viruses) . gType) groups
  selectionOrder = map fst $ sortBy (comparing snd) (M.toList groups)
  go ((wbcs', viruses'), targets') gId'
    | (gType g') == WhiteBloodCells && isNothing selectedVirusGId = ((wbcs', remainingViruses), targets')
    | (gType g') == Viruses && isNothing selectedWbcsGId = ((remainingWbcs, viruses'), targets')
    | (gType g') == WhiteBloodCells = ((wbcs', remainingViruses), M.insert gId' (fromJust selectedVirusGId) targets')
    | (gType g') == Viruses = ((remainingWbcs, viruses'), M.insert gId' (fromJust selectedWbcsGId) targets')
    where
      g' = groups M.! gId'
      (selectedVirusGId, remainingViruses) = selectTarget g' viruses'
      (selectedWbcsGId, remainingWbcs) = selectTarget g' wbcs'
  go _ _ = error "selectTargets: go: Unexpected pattern match."

-- | select a/the target for a given group from the map of availableEnemies.
selectTarget :: Group -> Groups -> (Maybe GId, Groups)
selectTarget attacker availableEnemies
  | M.null availableEnemies = (Nothing, M.empty)
  | otherwise = (enemyId', remainingEnemies')
  where
    possibleDamage = map (calcDamage'' attacker) (M.toList availableEnemies)
    calcDamage'' a (eId, e) = (calcDamage' a e , eId, e)
    (highestDamage, _, _) = head $ sortBy (comparing (Down . byDamage)) possibleDamage where
      byDamage (d, _, _) = d
    enemyId'
      | highestDamage == 0 = Nothing
      | otherwise = Just $ snd' $ head $ sortBy (comparing trd') $ filter ((== highestDamage) . fst') possibleDamage
      where
        fst' (d, _, _) = d
        snd' (_, eId, _) = eId
        trd' (_, _, g) = g
    remainingEnemies'
      | highestDamage == 0 = availableEnemies
      | otherwise = M.delete (fromJust enemyId') availableEnemies

-- | calculate the health of a group.
calcHealth :: Group -> Int
calcHealth g = (gUnits g) * (gHitpoints g)

-- | helper function to only insert into a map, if the key does not exist
-- yet (otherwise keep the old value)
onlyIfNotThere :: Group -> Group -> Group
onlyIfNotThere _ ov = ov

-- | update the groups.
updateGroups :: GId -> GId -> Maybe Group -> [GId] -> Groups -> Groups -> ([GId], Groups)
updateGroups aId eId Nothing remainingAttackerIds groups nextGroups = (remainingAttackerIds', nextGroups') where
  remainingAttackerIds' = delete eId remainingAttackerIds
  nextGroups' = M.delete eId $ M.insertWith onlyIfNotThere aId (groups M.! aId) nextGroups
updateGroups aId eId (Just g) remainingAttackerIds groups nextGroups = (remainingAttackerIds, nextGroups') where
  nextGroups' = M.insert eId g $ M.insertWith onlyIfNotThere aId (groups M.! aId) nextGroups
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
-- Note: This cannot be a fold. This needs to be a recursion, because I am changing
-- what is left over to iterate over (remainingAttackerIds).
--
fight :: Groups -> Targets -> Groups
fight groups targets = go attackingOrder M.empty where
  attackingOrder = map fst $ sortBy (comparing (Down . gInitiativeLevel . snd)) (M.toList groups)
  go (attackerId:[]) nextGroups
    | M.member attackerId targets = nextGroups'
    | otherwise = nextGroups''
    where
      defenderId = targets M.! attackerId
      attackedGroup
        | M.member attackerId nextGroups = attack (nextGroups M.! attackerId) (groups M.! defenderId)
        | otherwise = attack (groups M.! attackerId) (groups M.! defenderId)
      (_, nextGroups') = updateGroups attackerId defenderId attackedGroup [] groups nextGroups
      nextGroups'' = M.insertWith onlyIfNotThere attackerId (groups M.! attackerId) nextGroups
  go (attackerId:remainingAttackerIds) nextGroups
    | M.member attackerId targets = go remainingAttackerIds' nextGroups'
    | otherwise = go remainingAttackerIds nextGroups''
    where
      defenderId = targets M.! attackerId
      attackedGroup
        | M.member attackerId nextGroups = attack (nextGroups M.! attackerId) (groups M.! defenderId)
        | otherwise = attack (groups M.! attackerId) (groups M.! defenderId)
      (remainingAttackerIds', nextGroups') = updateGroups attackerId defenderId attackedGroup remainingAttackerIds groups nextGroups
      nextGroups'' = M.insertWith onlyIfNotThere attackerId (groups M.! attackerId) nextGroups
  go _ _ = error "fight: go: Unexpected pattern match."

-- | attack a target and return the damaged target (or nothing, if the target was destroyed).
attack :: Group -> Group -> Maybe Group
attack attacker defender
  | damage >= health = Nothing
  | otherwise = Just $ defender { gUnits = remainingUnits }
  where
    damage = calcDamage' attacker defender
    health = calcHealth defender
    remainingUnits = (gUnits defender) - killedUnits
    killedUnits = div (calcDamage' attacker defender) (gHitpoints defender)

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
