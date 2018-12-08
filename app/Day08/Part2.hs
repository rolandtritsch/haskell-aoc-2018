module Day08.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import qualified Data.Tree as T

import Day08

-- | solve the puzzle
solve :: [Int] -> Int
solve inputStream = go root where
  root = createRootNode inputStream
  go node
    | null (T.subForest node) = sum (T.rootLabel node)
    | otherwise = sum . map (go . (T.subForest node !!) . subtract 1) . filter (<= length (T.subForest node)) $ T.rootLabel node

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day08: Part2: Memory Maneuver: indexSum -> (%d, %f)\n" result time
