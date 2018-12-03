module Day02.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day02

-- | compare every boxid with every boxid and find
-- out what the difference is (and what is common :)).
diff :: [BoxId] -> [(BoxId, BoxId, String, String)]
diff (bid:rest) = concatMap diffId' rest where
  diffId' bid' = [(bid, bid', difference', common')] ++ diff rest where
    difference' = concat $ zipWith notSame bid bid' where
      notSame c0 c1 = if c0 /= c1 then [c0] else ""
    common' = concat $ zipWith same bid bid' where
      same c0 c1 = if c0 == c1 then [c0] else ""
diff [] = []

-- | solve the puzzle
solve :: [BoxId] -> String
solve boxIds = common where
  (_, _, _, common) = head $ filter diffByOne $ diff boxIds where
    diffByOne (bid', _, _, common') = (length bid') == ((length common') + 1)

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day02: Part2: Inventory Management System: diffByOne -> (%s, %f)\n" result time
