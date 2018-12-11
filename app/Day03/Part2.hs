module Day03.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import qualified Data.Map as M
import Data.List (sort, nub, (\\))

import Day03

-- | solve the puzzle
solve :: [Claim] -> Int
solve cs = nonOverlappingCid where
  nonOverlappingCid = go claimsCids where
    go [] = error "No cid found"
    go (cids:rest)
      | (length cids) == 1 && cidIsNotInAnyOtherList = head cids
      | otherwise = go rest
      where
        cidIsNotInAnyOtherList = (not $ any id $ map (elem (head cids)) ((\\) claimsCids [cids]))
  claimsCids = nub $ sort $ M.elems fabric
  fabric = foldl claim M.empty cs

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day03: No Matter How You Slice It: Part2: nonOverlappingCid -> (%d, %f)\n" result time
