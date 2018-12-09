{-|
Problem: <https://adventofcode.com/2018/day/9>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day09 where

import qualified Data.Map as M
import qualified Data.Sequence as S

import Util (inputRaw)

type Game = (Int, Int)
type Scores = M.Map Int Int
type Player = Int
type Board = (Int, Int, S.Seq Int)

data GameState = GameState Game Scores Player Board deriving (Show, Eq)

-- | read the input file
input :: Game
input = (numberOfPlayers, numberOfMarples) where
  tokens = words $ head $ inputRaw "input/Day09input.txt"
  numberOfPlayers = read $ tokens !! 0
  numberOfMarples = read $ tokens !! 6

-- | init the game.
initGame :: Game -> GameState
initGame game = GameState game M.empty 0 (1, 1, S.fromList [0, 1])

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
      nextField = S.insertAt nextPosition nextMarple field
      nextPosition'
        | position - 7 == 0 = (S.length field)
        | position - 7 == (negate 1) = (S.length field) - 1
        | position - 7 == (negate 2) = (S.length field) - 2
        | position - 7 == (negate 3) = (S.length field) - 3
        | position - 7 == (negate 4) = (S.length field) - 4
        | position - 7 == (negate 5) = (S.length field) - 5
        | position - 7 == (negate 6) = (S.length field) - 6
        | otherwise = position - 7
      nextField' = S.deleteAt nextPosition' field
  nextScores (np, nm, _)
    | (mod nm 23) == 0 = M.insertWith (+) (player + 1) (nm + S.index field np) scores
    | otherwise = scores