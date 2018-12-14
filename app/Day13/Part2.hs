module Day13.Part2 where

--import Debug.Trace

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import qualified Data.Map as M
--import qualified Data.Set as S

import Day13

-- | solve the puzzle
solve :: Tracks -> Position
solve tracks = go carts [] where
  (grid, carts) = buildGrid tracks
  go carts' collisions'
    -- | M.size carts' > 1 = trace ("*** - " ++ show (M.size carts')) $ traceShow carts' $ traceShow collisions' $ go (eliminate carts'' collisions'') (collisions' ++ collisions'')
    -- | otherwise = trace ("*** - " ++ show (M.size carts')) $ traceShow carts' $ traceShow collisions' $ fst $ head $ M.toList carts'
    | M.size carts' > 1 = go carts'' (collisions' ++ collisions'')
    | otherwise = head $ M.keys carts'
    where
      (carts'', collisions'') = tick grid carts'

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day13: Mine Cart Madness: Part2: final -> (%s, %f)\n" (show result) time
