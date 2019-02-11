{-|
Problem: <https://adventofcode.com/2018/day/3>

Solution:

General - Interesting. Half of the solution is to find a good/the right
datastructure to represent the fabric. In that context it is important
to realize that you need to know (and keep track of), which cids claimed
an inch.

Part 1 - Look at the fabric and find all inches that were claimed more
than once.

Part 2 - Look at the fabric and find the (first) inch that was only
written once AND check that this cid has never been used in an
overlapping area.
-}
module Day03 where

import Text.Megaparsec (manyTill, eof, optional)
import Text.Megaparsec.Char (newline, char, string)

import Util (inputRaw, inputRaw1, inputParser, Parser, integer)

import Data.List.Split (splitOneOf)
import qualified Data.Map as M

type Id = Int
type Position = (Int, Int)
type Dimension = (Int, Int)

data Claim = Claim Id Position Dimension deriving (Show, Eq)
type Claims = [Claim]

-- | Fabric position [cids that claimed that position]
type Fabric = M.Map Position [Id]

-- | read the input file.
input :: Claims
input = (map processLine . inputRaw) "input/Day03input.txt" where
  processLine l = Claim cid' cposition' cdimension' where
    -- #1 @ 335,861: 14x10
    tokens = splitOneOf "#@,:x " l
    cid' = read $ tokens !! 1
    cposition' = (read $ tokens !! 4, read $ tokens !! 5)
    cdimension' = (read $ tokens !! 7, read $ tokens !! 8)

-- | read the input file
input1 :: String
input1 = inputRaw1 "input/Day03input.txt"

-- | the parsed input.
parsedInput :: Claims
parsedInput = inputParser parseClaims "input/Day03input.txt"

parseClaims :: Parser Claims
parseClaims = manyTill (parseClaim <* optional newline) eof

parseClaim :: Parser Claim
parseClaim = Claim
  <$> parseClaimId
  <*> parseClaimPosition
  <*> parseClaimDimension

parseClaimId :: Parser Id
parseClaimId = char '#' *> integer

parseClaimPosition :: Parser Position
parseClaimPosition = (,) <$ string " @ " <*> integer <* char ',' <*> integer

parseClaimDimension :: Parser Dimension
parseClaimDimension = (,) <$ string ": " <*> integer <* char 'x' <*> integer

-- | make/stake a claim on an fabric area (by add the cid to the list of
-- cids that have claimed the inch sofar)
claim :: Fabric -> Claim -> Fabric
claim f (Claim cid (row, col) (rdim, cdim)) = foldl claimInch f cells where
  cells = [(r, c) | r <- [row..row + rdim - 1], c <- [col..col + cdim - 1]]
  claimInch f' p' = M.insert p' (cids ++ [cid]) f' where
    cids = M.findWithDefault [] p' f'
