module Day03.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import qualified Data.Map as M

import Day03

-- | solve the puzzle
solve :: [Claim] -> Int
solve cs = overlappingInches where
  overlappingInches = length $ filter ((<) 1) $ map length $ M.elems fabric
  fabric = foldl claim M.empty cs

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day03: No Matter How You Slice It: Part1: overlappingInches -> (%d, %f)\n" result time
