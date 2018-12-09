{-|
Problem: <https://adventofcode.com/2018/day/9>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day09 where

import qualified Data.Map as M

import Util (inputRaw)

type Game = (Int, Int)
type Scores = M.Map Int Int
type Player = Int
type Board = (Int, Int, [Int])

data GameState = GameState Game Scores Player Board deriving (Show, Eq)

-- | read the input file
input :: Game
input = (numberOfPlayers, numberOfMarples) where
  tokens = words $ head $ inputRaw "input/Day09input.txt"
  numberOfPlayers = read $ tokens !! 0
  numberOfMarples = read $ tokens !! 6

-- | init the game.
initGame :: Game -> GameState
initGame game = GameState game M.empty 0 (1, 1, [0, 1])

-- | make a move.
addMarple :: GameState -> GameState
addMarple (GameState game@(nop, _) scores player (position, marple, field)) = nextState where
  nextState = GameState game (nextScores nextBoard) nextPlayer nextBoard
  nextPlayer = mod (player + 1) nop
  nextMarple = marple + 1
  nextBoard
    | mod nextMarple 23 == 0 = (nextPosition', nextMarple, nextField')
    | otherwise = (nextPosition, nextMarple, nextField)
    where
      nextPosition
        | position + 2 == (length field) + 1 = 1
        | position + 2 == (length field) + 2 = 2
        | otherwise = position + 2
      nextField = take nextPosition field ++ [nextMarple] ++ drop nextPosition field
      nextPosition'
        | position - 7 == 0 = (length field)
        | position - 7 == (negate 1) = (length field) - 1
        | position - 7 == (negate 2) = (length field) - 2
        | position - 7 == (negate 3) = (length field) - 3
        | position - 7 == (negate 4) = (length field) - 4
        | position - 7 == (negate 5) = (length field) - 5
        | position - 7 == (negate 6) = (length field) - 6
        | otherwise = position - 7
      nextField' = take nextPosition' field ++ drop (nextPosition' + 1) field
  nextScores (np, nm, _)
    | (mod nm 23) == 0 = M.insertWith (+) (player + 1) (nm + (field !! np)) scores
    | otherwise = scores
