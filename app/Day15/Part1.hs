module Day15.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day15

-- | solve the puzzle
solve :: BattleGround -> Int
solve _ = 1
{-
solve battleGround = rounds * hitpoints where
  (rounds, units) = go 0 battleGround where
      go r bg@(_, units')
        | elfsWin || goblinsWin = (r, units')
        | otherwise = go (r + 1) (nextRound bg)
        where
          elfsWin = (null . filter (\(Unit t _ _) -> t == Goblin)) (M.elems units')
          goblinsWin = (null . filter (\(Unit t _ _) -> t == Elf)) (M.elems units')
  hitpoints = sum $ map (\(Unit _ _ hps) -> hps) (M.elems units)
-}
-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day15: Beverage Bandits: Part1: solve -> (%d, %f)\n" result time
