{-|
Problem: <https://adventofcode.com/2018/day/12>

Solution:

General - Tricky. Had a hard time to get the datastructure right.
Tried List, Array, Sequence, but settled on a Map (again).

That map can grow into both directions. With every evolution
I am checking for the smallest and the biggest pod and add two
empty pods on both sides (to make sure we can match the start
and end of the pods with the notes/rules).

Note: To make "....#" match you need 4 false pod holes at the
beginning of the list. I am adding 2, but I am also doing a/the
findWithDefault to "generate" other 2.

Part 1 - Easy. Just get all of the pod hole numbers that are
alive/inhabited and sum them up.

Part 2 - Hard. Had to get a/the clue on reddit. The sumOfPotNumbers
is a polynom. You *just* need to find the numbers that define
the polynom.
-}
module Day12 where

import Text.Megaparsec (many, eof, optional, (<|>))
import Text.Megaparsec.Char (newline, string, char)

import Util (inputRaw, inputRaw1, inputParser, Parser)

import Data.List (nub)
import Data.List.Split (splitOneOf)
import qualified Data.Map as M

type Pod = Int
type Note = [Bool]
type State = M.Map Pod Bool
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

-- | read the input file
input1 :: String
input1 = inputRaw1 "input/Day12input.txt"

-- | the parsed input.
parsedInput :: (State, Notes)
parsedInput = inputParser parseInit "input/Day12input.txt"

parseInit :: Parser (State, Notes)
parseInit = (,)
  <$> parseState
  <* newline
  <* newline
  <*> parseNotes
  <* eof

parseState :: Parser State
parseState = toState
  <$ string "initial state: "
  <*> many (parseAlive <|> parseDead)
  where
    toState states = M.fromList $ zip [0..] states

parseAlive, parseDead :: Parser Bool
parseAlive = True <$ char '#'
parseDead = False <$ char '.'

parseNotes :: Parser Notes
parseNotes = M.fromList <$> many (parseNote <* optional newline)

parseNote :: Parser (Note, Bool)
parseNote =  (,)
  <$> many (parseAlive <|> parseDead)
  <* string " => "
  <*> (parseAlive <|> parseDead)

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
detectShortcut gen notes state = go [] gen state where
  go stack' gen' state'
    | lastTenDiffsAreTheSame = (stack' !! 10, gen' + 11, (stack' !! 0) - (stack' !! 1))
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
