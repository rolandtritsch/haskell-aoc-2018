{- |
Problem: <https://adventofcode.com/2018/day/25>

Solution:

General - Last, but not least ... :).

The trick here is to realize that you can build a graph
of nodes and if you link the nodes both ways you can use
the "strongly connected components" (scc) function to
find the (number of) components (subgraphs) that are
connected.

Part 1 - Look for the number of sccs.

Part 2 - Merry Christmas.
-}
module Day25 where

import Text.Megaparsec (manyTill, eof, optional)
import Text.Megaparsec.Char (newline, string)

import Util (inputRaw, inputRaw1, inputParser, Parser, signedInteger)

type Point = (Int, Int, Int, Int)
type Points = [Point]

-- | read the input file
input :: [String]
input = inputRaw "input/Day25input.txt"

-- | read the input file (in one line)
input1 :: String
input1 = inputRaw1 "input/Day25input.txt"

-- | the parsed input.
parsedInput :: Points
parsedInput = inputParser parsePoints "input/Day25input.txt"

-- | parse the input.
parsePoints :: Parser Points
parsePoints = manyTill (parsePoint <* optional newline) eof

parsePoint :: Parser Point
parsePoint = (,,,)
  <$> signedInteger
  <* string ","
  <*> signedInteger
  <* string ","
  <*> signedInteger
  <* string ","
  <*> signedInteger

-- | the (manhattan) distance between two points.
distance :: Point -> Point -> Int
distance (w, x, y, z) (w', x', y', z') = (abs (w - w')) + (abs (x - x')) + (abs (y - y')) + (abs (z - z'))

-- | find all parent/child relationships (two points that have a distance of 3 (or less)).
edges :: Points -> [((), Point, Points)]
edges ps = foldl go [] ps where
  go es p = ((), p, pChildren) : es where
    pChildren = filter (byDistance 3) ps where
      byDistance d p'
        | distance p p' <= d = True
        | otherwise = False
