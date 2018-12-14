module Day11.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day11

-- | solve the puzzle
solve :: SerialNumber -> Coordinate
solve serial = coordinate where
  (coordinate, _, _) = largestTotalPowerLevel 3 $ buildPowerGrid serial

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day11: Chronal Charge: Part1: coordinate -> (%s, %f)\n" (show result) time
