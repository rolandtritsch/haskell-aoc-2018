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

import Text.Megaparsec (many, eof, optional)
import Text.Megaparsec.Char (newline, string, upperChar)

import Util (inputRaw, inputRaw1, inputParser, Parser)

import Data.List ((\\), sort)
import qualified Data.Map as M

type Step = Char
type Dependency = (Step, Step)
type Dependencies = M.Map Step [Step]
type Graph = (Dependencies, Dependencies)

type Duration = Int
type Work = M.Map Step Duration

-- | read the input file.
input :: [Dependency]
input = (map process . inputRaw) "input/Day07input.txt" where
  -- Step J must be finished before step E can begin.
  process line = (head $ words line !! 1, head $ words line !! 7)

-- | read the input file
input1 :: String
input1 = inputRaw1 "input/Day07input.txt"

-- | the parsed input.
parsedInput :: [Dependency]
parsedInput = inputParser parseDependencies "input/Day07input.txt"

parseDependencies :: Parser [Dependency]
parseDependencies = many (parseDependency <* optional newline) <* eof

parseDependency :: Parser Dependency
parseDependency = (,)
  <$ string "Step "
  <*> upperChar
  <* string " must be finished before step "
  <*> upperChar
  <* string " can begin."

-- | build a/the graph.
buildGraph :: [Dependency] -> Graph
buildGraph = foldl go (M.empty, M.empty) where
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
--
-- To elapse a second we need to ...
--
-- * take a look at what the work in progress and need to move
-- the tasks that are done from wip to finished (and need to
-- (re)sort finished.
-- * we then take the head of finished and add it to done.
-- * when then look for new tasks that can start (we add them to
-- available).
-- * we then take available tasks (we schedule tasks in order of
-- arrival (not by alphabet); FIFO; and as many as we have workers)
-- and add them to work in progress.
-- * last but not least we "tick" (decrement the remaining effort
-- by one) on all tasks that are work in progress and go to the
-- next second.
--
-- Initially ...
--
-- * nothing is done
-- * nothing is in progress
-- * all of the workers are available to take on work
-- * all of the roots are available to be schedule for work
--
-- We are done, when ...
--
-- * there are no more tasks available AND ...
-- * work in progress is empty (again) AND ...
-- * everything that was/is finished has been added to done
--
work :: Int -> Work -> Graph -> [Step] -> (Int, [Step])
work ws effort (parents, children) roots = go 0 [] [] [] roots ws where
  go :: Int -> [Step] -> [Step] -> [(Step, Duration)] -> [Step] -> Int -> (Int, [Step])
  go secs done [] [] [] _ = (secs - 1, done)
  go secs done finished wip available workers = go (secs + 1) done' finished''' (map tick wip') available' workers'
    where
      finished'''
        | null finished' = []
        | otherwise = tail finished'
      done'
        | null finished' = done
        | otherwise = done ++ [head finished']
      finished' = sort $ finished ++ finished''
      finished'' = map fst $ filter ((==) 0 . snd) wip
      workers'' = workers + length finished''
      available''
        | null done' = available
        | otherwise = sort $ available ++ foldl qualified [] (M.findWithDefault [] step children \\ (done' ++ (map fst $ wip)))
        where
          step = last done'
          qualified qs s
            | all (\p -> elem p done') (M.findWithDefault [] s parents) = qs ++ [s]
            | otherwise = qs
      availableForScheduling = take workers'' available''
      workers' = workers'' - length availableForScheduling
      wip' = filter ((/=) 0 . snd) wip ++ map schedule availableForScheduling where
        schedule s = (s, effort M.! s)
      available' = drop (length availableForScheduling) available''
      tick (s, e) = (s, e-1)
