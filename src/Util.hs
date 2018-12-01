{-|
Various utility functions.
-}
module Util where

import System.IO.Unsafe

-- | get the (raw) input (as a list of strings) from the given input file
inputRaw :: String -> [String]
inputRaw fileName = lines $ take (length contents - 1) contents
  where
    contents = unsafePerformIO $ readFile fileName
