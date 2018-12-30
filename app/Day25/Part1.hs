module Day25.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import qualified Data.Graph as G

import Day25

-- | solve the puzzle
solve :: Points -> Int
solve ps = length $ G.scc graph where
  (graph, _, _) = G.graphFromEdges $ edges ps

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve parsedInput)
  printf "Day25: Four-Dimensional Adventure: Part1: solve -> (%d, %f)\n" result time
