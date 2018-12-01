module Spec where

import Day00Spec
import Day01Spec

main :: IO ()
main = do
  putStrLn "\n--Day00Spec ..."
  Day00Spec.run
  putStrLn "\n--Day01Spec ..."
  Day01Spec.run
