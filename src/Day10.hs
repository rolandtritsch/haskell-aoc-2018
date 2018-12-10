{-|
Problem: <https://adventofcode.com/2018/day/10>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day10 where

--import Debug.Trace

import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.List (sort, minimumBy, maximumBy)
import Data.List.Split (splitOneOf)
import qualified Data.Sequence as S

import Util (inputRaw)

type Position = (Int, Int)
type Velocity = (Int, Int)
type Light = (Position, Velocity)
type Sky = S.Seq Light

-- | read the input file
input :: Sky
input = S.fromList $ sort $ map process $ inputRaw "input/Day10input.txt" where
  process line = ((x,y), (a,b)) where
    -- position=< 9,  1> velocity=< 0,  2>
    tokens = filter (not . null) $ splitOneOf "=<>, " line
    x = read $ tokens !! 1
    y = read $ tokens !! 2
    a = read $ tokens !! 4
    b = read $ tokens !! 5

-- | move the sky by one second.
tick :: Sky -> Sky
tick sky = foldl update sky [0..(S.length sky)] where
  update sky' light = S.update light ((x+a,y+b),(a,b)) sky' where
    ((x,y),(a,b)) = S.index sky' light

-- | return the dimensions of the sky.
dimension :: Sky -> ((Int, Int),(Int, Int))
dimension s = (fst $ minimumBy (comparing fst) s, fst $ maximumBy (comparing fst) s)

-- | moving through the night.
night :: Int -> Sky -> (Int, Sky)
night secs sky
  | iSeeSomething = (secs, sky)
  | otherwise = night (secs + 1) (tick sky)
  where
    ((minX, minY),(maxX, maxY)) = dimension sky
    iSeeSomething = (not . null . filter forVerticalLine) [minY..maxY] where
      forVerticalLine y = (maxX - minX + 1) == (S.length . S.filter (\((_, y'), _) -> y == y')) sky

-- | paint the lights on the sky :).
paint :: Sky -> [String]
paint sky = map paintLine [minX..maxX] where
  ((minX, minY),(maxX, maxY)) = dimension sky
  paintLine x = foldl paintLight "" [minY..maxY] where
    paintLight line y
      | isJust (S.findIndexL lightOn sky) = line ++ "*"
      | otherwise = line ++ " "
      where
        lightOn ((x',y'),_) = x == x' && y == y'
