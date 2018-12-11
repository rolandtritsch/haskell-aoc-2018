module Main where

import qualified Day01.Part1 as D01P1
import qualified Day01.Part2 as D01P2
import qualified Day02.Part1 as D02P1
import qualified Day02.Part2 as D02P2
import qualified Day03.Part1 as D03P1
import qualified Day03.Part2 as D03P2
import qualified Day04.Part1 as D04P1
import qualified Day04.Part2 as D04P2
import qualified Day05.Part1 as D05P1
import qualified Day05.Part2 as D05P2
import qualified Day06.Part1 as D06P1
import qualified Day06.Part2 as D06P2
import qualified Day07.Part1 as D07P1
import qualified Day07.Part2 as D07P2
import qualified Day08.Part1 as D08P1
import qualified Day08.Part2 as D08P2
import qualified Day09.Part1 as D09P1
--import qualified Day09.Part2 as D09P2
import qualified Day10.Part1 as D10P1
import qualified Day10.Part2 as D10P2
import qualified Day11.Part1 as D11P1
import qualified Day11.Part2 as D11P2

main :: IO ()
main = do
  D01P1.main
  D01P2.main
  D02P1.main
  D02P2.main
  D03P1.main
  D03P2.main
  D04P1.main
  D04P2.main
  D05P1.main
  D05P2.main
  D06P1.main
  D06P2.main
  D07P1.main
  D07P2.main
  D08P1.main
  D08P2.main
  D09P1.main
  --D09P2.main
  -- runs out of mem for/on eta.
  putStrLn "TODO - Day09: Dummy: Part2: dummy -> (0, 0)"
  D10P1.main
  D10P2.main
  D11P1.main
  D11P2.main
