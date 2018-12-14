{-|
Problem: <https://adventofcode.com/2018/day/13>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day13 where

import Prelude hiding (cos)
import qualified Data.Map as M

import Util (inputRaw)

data Position = Position Int Int deriving (Show, Eq, Ord)
data Segment = Horizontal | Vertical | TurnSlash | TurnBackslash | Intersection deriving (Show, Eq)
data Direction = Up' | Down' | Left' | Right' deriving (Show, Eq)

type Tracks = [String]
type Intersections = Int
type Grid = M.Map Position Segment
type Carts = M.Map Position (Direction, Intersections)

-- | read the input file
input :: Tracks
input = inputRaw "input/Day13input.txt"

-- | build the grid (from the tracks). Process every line and create segments from it.
buildGrid :: Tracks -> (Grid, Carts)
buildGrid tracks = foldl process (M.empty, M.empty) (zip [0..] tracks) where
  process (grid, carts) (row, l) = foldl segments (grid, carts) (zip [0..] l) where
    segments (g, c) (_, ' ') = (g, c)
    segments (g, c) (col, s)
      | elem s ['-'] = (M.insert (Position row col) Horizontal g, c)
      | elem s ['|'] = (M.insert (Position row col) Vertical g, c)
      | elem s ['/'] = (M.insert (Position row col) TurnSlash g, c)
      | elem s ['\\'] = (M.insert (Position row col) TurnBackslash g, c)
      | elem s ['+'] = (M.insert (Position row col) Intersection g, c)
      | elem s ['^'] = (M.insert (Position row col) Vertical g, M.insert (Position row col) (Up', 0) c)
      | elem s ['v'] = (M.insert (Position row col) Vertical g, M.insert (Position row col) (Down', 0) c)
      | elem s ['<'] = (M.insert (Position row col) Horizontal g, M.insert (Position row col) (Left', 0) c)
      | elem s ['>'] = (M.insert (Position row col) Horizontal g, M.insert (Position row col) (Right', 0) c)
    segments _ _ = error "segments: Unexpected pattern match."

-- | move the carts by one tick. Note: Given that we are moving them one after the other
-- we cannot look for collisions in the resulting cart positions, but need to check for
-- collisions every time we move a cart (and also return where these collisions occured).
--
-- The big problem is to detect the collisions.
--
-- What are the cases to consider ...
--
-- 1. before tick ->-<-; during tick --><-; after tick --X--; collision on 2
--   1. do not add the current cart and remove the other cart in current carts
-- 2. before tick -><-; during tick --X-; after tick -<>-; collision on 2 (not 1)
--   1. do not add the current cart and remove the other cart from previous carts
-- 3. before tick ->>-; during tick --X-; after tick -->>; collision on 2
--   1. do not add the current cart and remove the other cart from previous carts
-- 3. before tick -<<-; during tick <-<-; after tick <<--; NO collision
--
-- Also note that ...
--
-- * every collison needs to remove 2 carts; means if you start with an
-- even number of carts you will end up with 0 carts; if you start with
-- an odd number of carts you will end up with one cart.
-- * I do not understand, if there is a case for a 3-way or 4-way collision
-- and have not tested for it.
-- * because we move from left to right, the collision occurs on 2 (not on 1)
--
tick :: Grid -> Carts -> (Carts, [Position])
tick grid carts = (carts', collisions') where
  (carts', collisions') = M.foldlWithKey move (M.empty, []) carts where
    checkForCollison p cs = M.member p cs
    checkForCollison' p cos = elem p cos
{-    addCollision p cos cs carts''
      | M.member p cs = p : cos -- detect 1
      | M.member p carts'' = p : cos -- detect 2 and 3
      | otherwise = cos
-}
    --turn d p i cs cos = (M.insert p (d, i) cs, addCollision p cos cs carts)
    turn d p i cs = M.insert p (d, i) cs
    d4i Up' i
      | mod i 3 == 0 = Left'
      | mod i 3 == 1 = Up'
      | mod i 3 == 2 = Right'
    d4i Down' i
      | mod i 3 == 0 = Right'
      | mod i 3 == 1 = Down'
      | mod i 3 == 2 = Left'
    d4i Left' i
      | mod i 3 == 0 = Down'
      | mod i 3 == 1 = Left'
      | mod i 3 == 2 = Up'
    d4i Right' i
      | mod i 3 == 0 = Up'
      | mod i 3 == 1 = Right'
      | mod i 3 == 2 = Down'
    d4i _ _ = error "d4i: Unexpected pattern match"
    move (cs, cos) p@(Position row col) (Up', i)
      | checkForCollison p' carts = (cs, p' : cos)
      | checkForCollison p' cs = (M.delete p' cs, p' : cos)
      | checkForCollison' p cos = (cs, cos)
      | grid M.! p' == TurnSlash = (turn Right' p' i cs, cos)
      | grid M.! p' == TurnBackslash = (turn Left' p' i cs, cos)
      | grid M.! p' == Intersection = (turn (d4i Up' i) p' (i + 1) cs, cos)
      | otherwise = (turn Up' p' i cs, cos)
      where
        p' = Position (row - 1) col
    move (cs, cos) p@(Position row col) (Down', i)
      | checkForCollison p' carts = (cs, p' : cos)
      | checkForCollison p' cs = (M.delete p' cs, p' : cos)
      | checkForCollison' p cos = (cs, cos)
      | grid M.! p' == TurnSlash = (turn Left' p' i cs, cos)
      | grid M.! p' == TurnBackslash = (turn Right' p' i cs, cos)
      | grid M.! p' == Intersection = (turn (d4i Down' i) p' (i + 1) cs, cos)
      | otherwise = (turn Down' p' i cs, cos)
      where
        p' = Position (row + 1) col
    move (cs, cos) p@(Position row col) (Left', i)
      | checkForCollison p' carts = (cs, p' : cos)
      | checkForCollison p' cs = (M.delete p' cs, p' : cos)
      | checkForCollison' p cos = (cs, cos)
      | grid M.! p' == TurnSlash = (turn Down' p' i cs, cos)
      | grid M.! p' == TurnBackslash = (turn Up' p' i cs, cos)
      | grid M.! p' == Intersection = (turn (d4i Left' i) p' (i + 1) cs, cos)
      | otherwise = (turn Left' p' i cs, cos)
      where
        p' = Position row (col - 1)
    move (cs, cos) p@(Position row col) (Right', i)
      | checkForCollison p' carts = (cs, p' : cos)
      | checkForCollison p' cs = (M.delete p' cs, p' : cos)
      | checkForCollison' p cos = (cs, cos)
      | grid M.! p' == TurnSlash = (turn Up' p' i cs, cos)
      | grid M.! p' == TurnBackslash = (turn Down' p' i cs, cos)
      | grid M.! p' == Intersection = (turn (d4i Right' i) p' (i + 1) cs, cos)
      | otherwise = (turn Right' p' i cs, cos)
      where
        p' = Position row (col + 1)
