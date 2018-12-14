module Day11.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day11

-- | solve the puzzle
solve :: SerialNumber -> (Coordinate, Int)
solve serial = (coordinate, size) where
  (coordinate, _, size) = largestTotalPowerLevel' 300 $ buildPowerGrid' serial

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day11: Chronal Charge: Part2: size -> (%s, %f)\n" (show result) time
