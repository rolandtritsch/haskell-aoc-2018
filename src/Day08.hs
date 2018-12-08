{-|
Problem: <https://adventofcode.com/2018/day/0>

Solution:

General - Hard. Was clear that the Tree is the right
datastructure, but had a hard time to figure out how
to build it.

Part 1 - Summing up the metadata is simple.

Part 2 - That one was (very) tricky again.
-}
module Day08 where

import qualified Data.Tree as T

import Util (inputRaw)

type Metadata = [Int]

-- | read the input file.
input :: [Int]
input = map read $ words $ head $ inputRaw "input/Day08input.txt"

-- | create the (root) node (that will contain all other nodes).
createRootNode :: [Int] -> T.Tree Metadata
createRootNode inputStream = head $ snd $ go (inputStream, []) where
  go ((nOfChildren:nOfMetadata:stream), currentNode) = (drop nOfMetadata remainingStream, nextNode) where
    (remainingStream, children) = (iterate go (stream, [])) !! nOfChildren
    nextNode = T.Node (take nOfMetadata remainingStream) (reverse children) : currentNode
  go _ = error "Unexpected pattern match."
