{- |
Various utility functions.
-}
module Util where

import Data.Void
import System.IO.Unsafe

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = P.Parsec Void String

-- | get the (raw) input (as a list of strings) from the given input file
inputRaw :: String -> [String]
inputRaw fileName = lines $ take (length contents - 1) contents
  where
    contents = unsafePerformIO $ readFile fileName

-- | get the (raw) input (as one line (with newlines in it))
inputRaw1 :: String -> String
inputRaw1 fileName = take (length contents - 1) contents
  where
    contents = unsafePerformIO $ readFile fileName

-- | parse a signed integer.
signedInteger :: Parser Int
signedInteger = L.signed (return ()) L.decimal
