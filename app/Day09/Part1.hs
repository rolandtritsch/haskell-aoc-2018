module Day09.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import qualified Data.Map as M

import Day09

-- | solve the puzzle
solve :: Game -> Int
solve game@(_, numberOfMarples) = highScore where
  play = iterate addMarple (initGame game)
  (GameState _ scores _ _) = play !! (numberOfMarples - 1)
  highScore = maximum (M.elems scores)

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day09: Marble Mania: Part1: small -> (%d, %f)\n" result time
