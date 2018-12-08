{-|
Problem: <https://adventofcode.com/2018/day/7>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day07 where

import Data.List ((\\), sort)
import qualified Data.Map as M

import Util (inputRaw)

type Step = Char
type Dependency = (Step, Step)
type Dependencies = M.Map Step [Step]
type Graph = (Dependencies, Dependencies)

-- | read the input file.
input :: [Dependency]
input = map process $ inputRaw "input/Day07input.txt" where
  process line = (head $ words line !! 1, head $ words line !! 7)

-- | build a/the graph.
buildGraph :: [Dependency] -> Graph
buildGraph ds = foldl go (M.empty, M.empty) ds where
  go (parents, children) (from, to) = (parents', children') where
    parents' = M.insertWith (++) to [from] parents
    children' = M.insertWith (++) from [to] children

-- | find the root step(s) (by finding the step(s) with no parents).
findRoots :: Graph -> [Step]
findRoots (parents, children) = M.keys children \\ M.keys parents

-- | find the path (with/from the given roots).
findPath :: Graph -> [Step] -> [Step]
findPath (parents, children) roots = go [head roots] (tail roots) where
  go path available
    | null next = path
    | otherwise = go (path ++ [head next]) (tail next)
    where
      step = last path
      next = sort $ available ++ foldl qualified [] (M.findWithDefault [] step children \\ path) where
        qualified qs s
          | all (\p -> elem p path) (M.findWithDefault [] s parents) = qs ++ [s]
          | otherwise = qs
