{-|
Problem: <https://adventofcode.com/2018/day/11>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day11 where

import Data.List (maximumBy)
import qualified Data.Map as M

--import Util (inputRaw)

type SerialNumber = Int
type PowerLevel = Int
type Coordinate = (Int, Int)
type Grid = M.Map Coordinate PowerLevel

-- | read the input.
input :: SerialNumber
--input = inputRaw "input/Day11input.txt"
input = 3999

-- | the dimension of the grid.
dimension :: ((Int, Int),(Int, Int))
dimension = ((minCol, maxCol), (minRow, maxRow)) where
  minCol = 1
  maxCol = 300
  minRow = 1
  maxRow = 300

-- | set the power level of the fuel cells in the grid.
buildPowerGrid :: SerialNumber -> Grid
buildPowerGrid serial = foldl setPowerLevelOfFuelCell M.empty coordinates where
  ((minCol, maxCol), (minRow, maxRow)) = dimension
  coordinates = [(c, r) | c <- [minCol..maxCol], r <- [minRow..maxRow]]
  setPowerLevelOfFuelCell grid coordinate@(x, y) = M.insert coordinate powerLevel grid where
    powerLevel = hundredTh (((rackId * y) + serial) * rackId) - 5 where
      rackId = x + 10
      hundredTh n
        | length n' < 3 = 0
        | otherwise = read [n' !! ((length n') - 3)]
        where
          n' = show n

-- | return the largest power level (and its location) (up to the given max grid size).
largestTotalPowerLevel :: Int -> Grid -> (Coordinate, PowerLevel, Int)
largestTotalPowerLevel maxPowerGrid grid = maximumBy byPowerLevel totalPowerLevelsByAllGrids where
  byPowerLevel (_, p, _) (_, p', _) = compare p p'
  ((minCol, maxCol), (minRow, maxRow)) = dimension
  totalPowerLevelsByAllGrids = concatMap totalPowerLevelsByCoordinate [1..maxPowerGrid] where
    totalPowerLevelsByCoordinate n = map totalPower coordinates where
      coordinates = [(c, r) | c <- [minCol..maxCol-n+1], r <- [minRow..maxRow-n+1]]
      totalPower coordinate@(c, r) = (coordinate, total, n) where
        total = foldl sumCol 0 [0..n-1] where
          sumCol s' n' = s' + foldl sumRow 0 [0..n-1] where
            sumRow s'' n'' = s'' + grid M.! (c+n', r+n'')
