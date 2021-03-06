{-|
Problem: <https://adventofcode.com/2018/day/15>

Solution:

General - Let's first think about the types of operations/access
we need to perform. We probably need to ...

* iterate over all Goblins and Elfs (in reading order) and need
to have them move and then attack.
  * to move them we need to update their position.
    * to update their position we need to know, where the walls and
    caverns are (but we never need to update the walls and caverns :)).
  * to attack we need to find/access the unit (by its position) that gets
  attacked and need to update/decrease the hit points of that unit

Could probably do this with a map (or a couple of maps), but want to
learn about arrays. Was first thinking to use two arrays: One for the
field and one for the units. The one for the field is unmutable. But
array of units is mutable and sparsely populated. And ... we need to
not only update the cells (the units), but also their positions, which
makes array an akward datastructure (e.g. you cannot delete something
from an array).

Changing my mind: Two maps it is. Together they form the battleground.

Part 1 - ???

Part 2 - ???
-}
module Day15 where

import Text.Megaparsec (many, manyTill, eof, optional, (<|>), getSourcePos)
import Text.Megaparsec.Pos (SourcePos, sourceLine, sourceColumn, unPos)
import Text.Megaparsec.Char (newline, char)

import Util (inputRaw, inputRaw1, inputParser, Parser)

import Debug.Trace
--import Text.Printf

import Data.List (sort)
import Data.Maybe (isJust, fromJust, mapMaybe)
import Safe (headMay)
import qualified Data.Map as M

data Field = Wall | Open deriving (Show, Eq)
data UType = Goblin | Elf deriving (Show, Eq)
data Unit = Unit UType Int Int deriving (Show, Eq)
data Direction = North | South | East | West deriving (Show, Eq)

type Position = (Int, Int)
type Fields = M.Map Position Field
type Units = M.Map Position Unit
type BattleGround = (Fields, Units)

-- types for parsing.
type Fields' = (Position, Field)
type Units' = (Position, Unit)

-- | read the input file
input :: [String]
input = inputRaw "input/Day15input.txt"

-- | prepare the battle ground.
initialBattleground :: [String] -> BattleGround
initialBattleground rows = (M.fromList fields, M.fromList units) where
  rDim = (length rows) - 1
  cDim = (length $ head rows) - 1
  fields = [((r,c), (theField r c)) | r <- [0..rDim], c <- [0..cDim]] where
    theField r' c' = map' ((rows !! r') !! c') where
      -- you can move everywhere that is not a wall
      map' '#' = Wall
      map' '.' = Open
      map' 'G' = Open
      map' 'E' = Open
      map' _ = error "prepare: Unexpected pattern match."
  units = filter (((/=) (Unit Elf 0 0) . snd)) [((r,c), (theUnit r c)) | r <- [0..rDim], c <- [0..cDim]] where
    theUnit r' c' = map' ((rows !! r') !! c') where
      -- creating dummy elfs and filter them out later again.
      map' '#' = Unit Elf 0 0
      map' '.' = Unit Elf 0 0
      map' 'G' = Unit Goblin 3 200
      map' 'E' = Unit Elf 3 200
      map' _ = error "prepare: Unexpected pattern match."

-- | read the input file
input1 :: String
input1 = inputRaw1 "input/Day15input.txt"

-- | the parsed input.
parsedInput :: BattleGround
parsedInput = inputParser parseInit "input/Day15input.txt"

parseInit :: Parser BattleGround
parseInit = toBattleGround <$> manyTill (parseLine <* optional newline) eof where
  toBattleGround lines' = (M.fromList fields, M.fromList units) where
    lines'' = concat lines'
    fields = map fst lines''
    units = mapMaybe snd lines''

parseLine :: Parser [(Fields', Maybe Units')]
parseLine = many (parseWall <|> parseOpen <|> parseElf <|> parseGoblin)

parseWall, parseOpen, parseElf, parseGoblin :: Parser (Fields', Maybe Units')
parseWall = toWall <$> getSourcePos <* char '#' where
  toWall sp = ((toPosition sp, Wall), Nothing)
parseOpen = toOpen <$> getSourcePos <* char '.' where
  toOpen sp = ((toPosition sp, Open), Nothing)
parseElf = toElf <$> getSourcePos <* char 'E' where
  toElf sp = ((toPosition sp, Open), Just $ (toPosition sp, Unit Elf 3 200))
parseGoblin = toGoblin <$> getSourcePos <* char 'G' where
  toGoblin sp = ((toPosition sp, Open), Just $ (toPosition sp, Unit Goblin 3 200))

toPosition :: SourcePos -> Position
toPosition sp = (row, col) where
  row = (unPos (sourceLine sp)) - 1
  col = (unPos (sourceColumn sp)) - 1

-- | move a unit. But only, if the unit is not already in an attack position.
move :: BattleGround -> Units -> Position -> Unit -> Units
move bg@(_, units) currentUnits position unit
  | isJust $ checkForTarget units position = currentUnits
  | otherwise = M.insert (nextPosition bg position) unit currentUnits

-- | given a position, determine the next position to go to.
--
-- This is tricky ...
--
-- 1. we need to find all foes that are reachable (means we can get to it without
-- going through a wall or an elf or a goblin :)).
-- 2. then for all 4 direction positions we need to calc the manhatten distance to all of
-- these foes (nextPos, distance).
-- 3. then we need to find the nextPos with the smallest distance (and if there is more
-- than one pick, the smallest position).
--
nextPosition :: BattleGround -> Position -> Position
nextPosition (fields, units) position = trace "***" $ traceShow position $ traceShow np $ traceShow distances $ np where
  np = (fst . minimum . filter ((==) minDistance . snd)) distances
  allDirections
    = [moveNorth position]
    ++ [moveSouth position]
    ++ [moveWest position]
    ++ [moveEast position]
  allAvailableDirections = filter (\p -> fields M.! p == Open) allDirections
  (Unit utype _ _) = units M.! position
  allReachableFoes = go [] [] position where
    go alreadySeenPositions alreadyFoundFoePositions position'
      | elem position' alreadySeenPositions = alreadyFoundFoePositions
      | elem position' alreadyFoundFoePositions = alreadyFoundFoePositions
      | fields M.! position' == Wall = alreadyFoundFoePositions
      | M.member position' units && utype == utype' && position /= position' = alreadyFoundFoePositions
      | M.member position' units && utype /= utype' = position' : alreadyFoundFoePositions
      | fields M.! position' == Open = trace "---" $ traceShow position' $ traceShow alreadyFoundFoePositions $ traceShow alreadySeenPositions
        $ go (position':alreadySeenPositions) alreadyFoundFoePositions (moveNorth position')
        ++ go (position':alreadySeenPositions) alreadyFoundFoePositions (moveSouth position')
        ++ go (position':alreadySeenPositions) alreadyFoundFoePositions (moveWest position')
        ++ go (position':alreadySeenPositions) alreadyFoundFoePositions (moveEast position')
      | otherwise = error ("nextPosition: go: Unexpected pattern match: " ++ show position')
        where
          (Unit utype' _ _) = units M.! position'
  distances = [(p, distance p p') | p <- allAvailableDirections, p' <- allReachableFoes] where
    distance (r, c) (r', c') = abs (r - r') + abs (c - c')
  minDistance = minimum $ map snd distances

-- | attack a unit (if there is on to attack).
attack :: BattleGround -> Units -> Position -> Unit -> Units
attack (_, units) currentUnits position unit = attack' (checkForTarget units position) where
  attack' Nothing = M.insert position unit currentUnits
  attack' (Just attackPosition)
    | killedIt attackUnit unit = M.insert position unit currentUnits
    | otherwise = M.insert position unit $ M.insert attackPosition (tookAHit attackUnit unit) currentUnits
    where
      attackUnit = units M.! attackPosition
      killedIt (Unit _ _ h) (Unit _ p' _) = (h - p') <= 0
      tookAHit (Unit t p h) (Unit _ p' _) = Unit t p (h - p')

-- | check, if the units are foes.
isFoe :: Unit -> Unit -> Bool
isFoe (Unit t _ _) (Unit t' _ _) = t /= t'

-- | check, if the unit can attack another unit and if so, which one.
checkForTarget :: Units -> Position -> Maybe Position
checkForTarget units position = headMay $ sort $ map fst availableTargets where
  unit = units M.! position
  targets
    = (moveNorth position, M.lookup (moveNorth position) units)
    : (moveSouth position, M.lookup (moveSouth position) units)
    : (moveWest position, M.lookup (moveWest position) units)
    : (moveEast position, M.lookup (moveEast position) units)
    : []
  foeTargets = filter (isFoe unit . snd) $ map (\(p, u) -> (p, fromJust u)) $ filter (isJust . snd) targets
  availableTargets = filter forMinHitPoints foeTargets where
    forMinHitPoints (_, (Unit _ _ hps)) = hps == minHps
    minHps = minimum $ map forHps foeTargets where
      forHps (_, (Unit _ _ hps)) = hps

-- | return the next round. Move all units (and/or execute an attack).
nextRound :: BattleGround -> BattleGround
nextRound bg@(fields, units) = (fields, nextUnits) where
  nextUnits = trace "+++" $ traceShow units $ M.foldlWithKey (move bg) M.empty units
  --nextUnits = M.foldlWithKey (attack bg) M.empty nextPositions
  --nextPositions = M.foldlWithKey (move bg) M.empty units

-- | moving around.
moveNorth, moveSouth, moveEast, moveWest :: Position -> Position
moveNorth (row, col) = (row - 1, col)
moveSouth (row, col) = (row + 1, col)
moveWest (row, col) = (row, col - 1)
moveEast (row, col) = (row, col + 1)
