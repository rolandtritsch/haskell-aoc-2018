module Day06.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import qualified Data.Map as M

import Day06

-- | solve the puzzle
solve :: [Origin] -> Int
solve os = size where
  size = length $ filter (\r -> r == (1,1)) $ M.elems $ snd $ buildGrid' (Boundary 0 500 0 500) os

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day06: Part2: Chronal Coordinates: region -> (%d, %f)\n" result time
