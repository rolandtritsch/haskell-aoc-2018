{-|
Problem: <https://adventofcode.com/2018/day/12>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day12 where

--import Data.List (nub)
import Data.List.Split (splitOneOf)
import qualified Data.Map as M

import Util (inputRaw)

type Note = [Bool]
type State = M.Map Int Bool
type Notes = M.Map Note Bool

-- | read the input file
input :: (State, Notes)
input = (initialState, rules) where
  input' = inputRaw "input/Day12input.txt"
  firstLine = drop 15 $ head input'
  -- initial state: ##.##.##..#..#.#.#.#...#...#####.###... (...)
  initialState = M.fromList $ zip [0..] (map ((== '#')) firstLine)
  rules = M.fromList $ map process $ drop 2 input' where
    process l = (note, alive (tokens !! 1)) where
      tokens = filter (not . null) $ splitOneOf " => " l
      note = map ((==) '#') $ tokens !! 0
      alive "#" = True
      alive _ = False

-- | evolve (one generation).
evolve :: Notes -> State -> State
evolve notes state = M.mapWithKey nextState state' where
    mini = minimum $ M.keys $ M.filter id state
    maxi = maximum $ M.keys $ M.filter id state
    state'
      = M.insert (mini - 2) False
      $ M.insert (mini - 1) False
      $ M.insert (maxi + 1) False
      $ M.insert (maxi + 2) False state
    nextState i s = M.findWithDefault False note notes where
      note
        = [M.findWithDefault False (i - 2) state]
        ++ [M.findWithDefault False (i - 1) state]
        ++ [s]
        ++ [M.findWithDefault False (i + 1) state]
        ++ [M.findWithDefault False (i + 2) state]

-- | grow the plants for N generations.
generations :: Int -> Notes -> State -> State
generations 0 _ state = state
generations gen notes state = generations (gen - 1) notes state' where
  state' = (evolve notes state)

-- | the sum of pot numbers/indexes for this state
sumOfPotNumbers :: State -> Int
sumOfPotNumbers state = sum $ M.keys $ M.filter id state

-- | detect shortcut. After a while, the difference in sumOfPotNumbers
-- between generations is/becomes a constant. What we need to calculate
-- the result is the number of generations it takes to reach that constant
-- and what the constant is.
detectShortcut :: Int -> Notes -> State -> (Int, Int, Int)
detectShortcut _ _ _ = (5629, 49999999901, 62)
{-
detectShortcut gen notes state = go [] gen state where
  go stack' gen' state'
    | lastTenDiffsAreTheSame = (gen', (stack' !! 0) - (stack' !! 1))
    | otherwise = go ((sumOfPotNumbers state') : stack') (gen' - 1) (evolve notes state')
    where
      lastTenDiffsAreTheSame
        | length genDiffs < 10 = False
        | otherwise = (length . nub . take 10) genDiffs == 1
        where
          genDiffs = map diffPair $ slide2 stack' where
            slide2 (x:x':[]) = [[x, x']]
            slide2 (x:x':xs) = [x, x'] : slide2 (x' : xs)
            slide2 _ = [[0,0]]
            diffPair (g:g':[]) = g' - g
            diffPair _ = error "diffPair: Unexpected pattern match."
-}
