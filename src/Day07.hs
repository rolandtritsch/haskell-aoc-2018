{-|
Problem: <https://adventofcode.com/2018/day/7>

Solution:

General - That was a hard one. Initially I got the
graph wrong. Took me a while to realize that I need
to understand what are the children of the current
step AND what are the parents of the current step.

Part 1 - The challenge here is to make sure that
the nextSteps only become available after the
successors are done.

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

type Duration = Int
type Work = M.Map Step Duration

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

-- | build the map of effort (how much time does it take
-- to finish a step/task).
buildEffort :: Int -> Work
buildEffort delay = foldl go M.empty (zip ['A'..'Z'] [(1+delay)..]) where
  go w (s, d) = M.insert s d w

-- | work through the steps (with the given number of workers and
-- the given effort(s)) and return the number of seconds it took.
-- Initially ...
--
-- * nothing is done
--
--
--
work :: Int -> Work -> Graph -> [Step] -> Int
work ws effort (parents, children) roots = go 0 [] [] roots ws where
  go secs done inProgress available workers
    | null inProgress && null available = secs
    | otherwise = go (secs + 1) (done ++ [head finished]) inProgress' available' workers'
    where
      inProgress' = M.elems effort
      workers' = workers
      available' = undefined
      step = last done
      finished = sort $ available ++ foldl qualified [] (M.findWithDefault [] step children \\ done) where
        qualified qs s
          | all (\p -> elem p done) (M.findWithDefault [] s parents) = qs ++ [s]
          | otherwise = qs
