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

import Data.List (sort)
import Data.Maybe (isJust, isNothing, fromJust)
import Safe (headMay)
import qualified Data.Map as M

import Util (inputRaw)

data Field = Wall | Open deriving (Show, Eq)
data UType = Goblin | Elf deriving (Show, Eq)
data Unit = Unit UType Int Int deriving (Show, Eq)
data Direction = North | South | East | West deriving (Show, Eq)

type Position = (Int, Int)
type Fields = M.Map Position Field
type Units = M.Map Position Unit
type BattleGround = (Fields, Units)

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

-- | move a unit. But only, if the unit is not already in an attack position.
move :: Units -> Units -> Position -> Unit -> Units
move previousUnits currentUnits position unit
  | isNothing $ checkForTarget previousUnits position = currentUnits
  | otherwise = M.insert nextPosition unit currentUnits
    where
      nextPosition = position -- TODO :)

-- | attack a unit (if there is on to attack).
attack :: Units -> Units -> Position -> Unit -> Units
attack previousUnits currentUnits position unit = attack' (checkForTarget previousUnits position) where
  attack' Nothing = M.insert position unit currentUnits
  attack' (Just attackPosition)
    | killedIt attackUnit unit = M.insert position unit currentUnits
    | otherwise = M.insert position unit $ M.insert attackPosition (tookAHit attackUnit unit) currentUnits
    where
      attackUnit = previousUnits M.! attackPosition
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
nextRound (fields, units) = (fields, nextUnits) where
  nextUnits = M.foldlWithKey (attack units) M.empty nextPositions
  nextPositions = M.foldlWithKey (move units) M.empty units

-- | moving around.
moveNorth, moveSouth, moveEast, moveWest :: Position -> Position
moveNorth (row, col) = (row - 1, col)
moveSouth (row, col) = (row + 1, col)
moveWest (row, col) = (row, col - 1)
moveEast (row, col) = (row, col + 1)
