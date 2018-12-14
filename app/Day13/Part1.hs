module Day13.Part1 where

-- import Debug.Trace

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

--import qualified Data.Map as M
--import qualified Data.Set as S

import Day13

-- | solve the puzzle.
solve :: Tracks -> Position
solve tracks = go carts [] where
  (grid, carts) = buildGrid tracks
  -- go carts' [] = trace ("### - " ++ show (M.size carts')) $ traceShow carts' $ trace "[]" $ go (eliminate carts'' collisions'') collisions'' where
  go carts' [] = go carts'' collisions'' where
    (carts'', collisions'') = tick grid carts'
  -- go carts' collisions'@(p:_) = trace ("### - " ++ show (M.size carts')) $ traceShow carts' $ traceShow collisions' $ p
  go _ (p:_) = p

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day13: Mine Cart Madness: Part1: first -> (%s, %f)\n" (show result) time
