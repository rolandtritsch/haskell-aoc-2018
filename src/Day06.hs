{-|
Problem: <https://adventofcode.com/2018/day/6>

Solution:

General - Tricky. Initially I had no idea how to go about this. Had
to take a nap first and then I realized that ...

- the manhatten distance of a point p (x,y) from an origin (0,0)
is abs(x) + abs(y) and the distance between two points p0 and p1
is abs(x0-x1)+(y0-y1). We can use this to calculate the distances
from the origins that are given to us (origin + px).

Means I can build a grid ...

- and for every point/coordinate in that grid
- I can take all origins and calculate the distance to that origin
- I can then take the minimum and know which origin is the closest
  - Note: If there is more than one minimum, I cannot decide which
  origin is the closest.

After that ...

- I find all origin areas that touch the boundary of the grid and ...
- ... remove them from consideration for finding the largest area.

The rest is easy/simple: Look for the most coordinates that are
associated with an origin.

Part 1 - ???

Part 2 - ???
-}
module Day06 where

import Text.Megaparsec (many, eof, optional)
import Text.Megaparsec.Char (newline, string)

import Util (inputRaw, inputRaw1, inputParser, Parser, integer)

import Data.Ord (comparing)
import Data.List (minimumBy, nub, groupBy, maximumBy, sortBy)
import Data.List.Split (splitOneOf)
import qualified Data.Map as M

type Coordinate = (Int, Int)
type Origin = (Int, Int)
type Distance = Int

-- | the map/grid to show, which coordinate is closest to which
-- origin (many to one; you can say that a/the given coordinate
-- is *owned* by the origin. Note: If there is no *winner*, means
-- the cooridnate is closest to more than one origin (no minimum),
-- then I am not putting that coordinate into the map/onto the
-- grid.
type Grid = (Boundary, M.Map Coordinate Origin)

data Boundary = Boundary {
  minX :: Int,
  maxX :: Int,
  minY :: Int,
  maxY :: Int
  }

-- | read the input file.
input :: [Origin]
input = map line $ inputRaw "input/Day06input.txt" where
  line l = (x, y) where
    tokens = splitOneOf "," l
    x = read $ tokens !! 0
    y = read $ tokens !! 1

-- | read the input file
input1 :: String
input1 = inputRaw1 "input/Day06input.txt"

-- | the parsed input.
parsedInput :: [Origin]
parsedInput = inputParser parseOrigins "input/Day06input.txt"

parseOrigins :: Parser [Origin]
parseOrigins = many (parseOrigin <* optional newline) <* eof

parseOrigin :: Parser Origin
parseOrigin = (,) <$> integer <* string ", " <*> integer

-- | find the origin that is closest to the given point.
closestOrigin :: Coordinate -> [Origin] -> Maybe Origin
closestOrigin (x, y) os
  | (length minimums) == 1 = Just (fst $ head minimums)
  | (length minimums) >= 1 = Nothing
  | otherwise = error "This should not happen."
  where
    minimums = filter (\(_, d) -> d == minDistance) distances where
      minDistance = snd $ minimumBy (comparing snd) distances
      distances = map distance os where
        distance o@(xo, yo) = (o, abs (x - xo) + abs (y - yo))

-- | build the grid.
buildGrid :: Boundary -> [Origin] -> Grid
buildGrid b origins = (b, foldl go M.empty coordinates) where
  -- | checked the input. My grid has no negative values in it.
  coordinates = [(x,y) | x <- [(minX b)..(maxX b)], y <- [(minY b)..(maxY b)]]
  go grid c = insert (closestOrigin c origins) where
    insert Nothing = grid
    insert (Just o) = M.insert c o grid

-- | get the origins that touch the boundary of the grid.
infinite :: Grid -> [Origin]
infinite (b, grid) = nub $ foldl notNothing [] originsOnTheBoundary where
  notNothing os Nothing = os
  notNothing os (Just o) = o:os
  originsOnTheBoundary
    = [grid M.!? c | x <- [(minX b)..(maxX b)], let c = (x,(minY b))]
    ++ [grid M.!? c | x <- [(minX b)..(maxX b)], let c = (x,(maxY b))]
    ++ [grid M.!? c | y <- [(minY b)..(maxY b)], let c = ((minX b),y)]
    ++ [grid M.!? c | y <- [(minY b)..(maxY b)], let c = ((maxX b),y)]

-- | get the largest area (that is not infinite).
largest :: Grid -> ((Int, Int), Int)
largest (b, grid) = maximumBy (comparing snd) areas where
  areas = map size $ groupBy byOrigin $ sortBy (comparing snd) $ filter notInfinite (M.toList grid) where
    ios = infinite (b, grid)
    notInfinite (_, o) = not $ elem o ios
    byOrigin (_, o) (_, o') = (==) o o'
    size a = (snd $ head a, length a)

-- | build the grid.
buildGrid' :: Boundary -> [Origin] -> Grid
buildGrid' b origins = (b, foldl go M.empty coordinates) where
  -- | checked the input. My grid has no negative values in it.
  coordinates = [(x,y) | x <- [(minX b)..(maxX b)], y <- [(minY b)..(maxY b)]]
  go grid c@(x, y) = M.insert c (lessThan 10000) grid where
    lessThan n
      | d < n = (1,1)
      | otherwise = (0,0)
      where
        d = foldl distances 0 origins where
          distances s (xo, yo) = s + abs (x - xo) + abs (y - yo)
