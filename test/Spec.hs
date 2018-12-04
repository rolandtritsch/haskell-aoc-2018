module Spec where

import Day00Spec
import Day01Spec
import Day02Spec
import Day03Spec
import Day04Spec
import Day05Spec

main :: IO ()
main = do
  putStrLn "\n--Day00Spec ..."
  Day00Spec.run
  putStrLn "\n--Day01Spec ..."
  Day01Spec.run
  putStrLn "\n--Day02Spec ..."
  Day02Spec.run
  putStrLn "\n--Day03Spec ..."
  Day03Spec.run
  putStrLn "\n--Day04Spec ..."
  Day04Spec.run
  putStrLn "\n--Day05Spec ..."
  Day05Spec.run
