{-|
Problem: <https://adventofcode.com/2018/day/13>

Solution:

General - Jesus. I really struggled with this one. Building the grid
is straight forward. Moving the carts around is straight forward.
The collision detection was killing me.

Part 1 - We *just* execute ticks until we find/run into the first collision.

Part 2 - We *just* execute ticks until only one cart is left over.
-}
module Day13 where

import Text.Megaparsec (many, manyTill, eof, optional, (<|>), getSourcePos)
import Text.Megaparsec.Pos (SourcePos, sourceLine, sourceColumn, unPos)
import Text.Megaparsec.Char (newline, char, space)

import Util (inputRaw, inputRaw1, inputParser, Parser)

import Data.Maybe (mapMaybe)
import Prelude hiding (cos)
import qualified Data.Map as M

data Position = Position Int Int deriving (Show, Eq, Ord)
data Segment = Horizontal | Vertical | TurnSlash | TurnBackSlash | Intersection deriving (Show, Eq)
data Direction = Up' | Down' | Left' | Right' deriving (Show, Eq)

type Tracks = [String]
type Intersections = Int
type Grid = M.Map Position Segment
type Carts = M.Map Position (Direction, Intersections)

-- types for parsing
type Grid' = (Position, Segment)
type Carts' = (Position, (Direction, Intersections))

-- | read the input file
input :: (Grid, Carts)
input = buildGrid $ inputRaw "input/Day13input.txt"

-- | build the grid (from the tracks). Process every line and create segments from it.
buildGrid :: Tracks -> (Grid, Carts)
buildGrid tracks = foldl process (M.empty, M.empty) (zip [0..] tracks) where
  process (grid, carts) (row, l) = foldl segments (grid, carts) (zip [0..] l) where
    segments (g, c) (_, ' ') = (g, c)
    segments (g, c) (col, s)
      | elem s ['-'] = (M.insert (Position row col) Horizontal g, c)
      | elem s ['|'] = (M.insert (Position row col) Vertical g, c)
      | elem s ['/'] = (M.insert (Position row col) TurnSlash g, c)
      | elem s ['\\'] = (M.insert (Position row col) TurnBackSlash g, c)
      | elem s ['+'] = (M.insert (Position row col) Intersection g, c)
      | elem s ['^'] = (M.insert (Position row col) Vertical g, M.insert (Position row col) (Up', 0) c)
      | elem s ['v'] = (M.insert (Position row col) Vertical g, M.insert (Position row col) (Down', 0) c)
      | elem s ['<'] = (M.insert (Position row col) Horizontal g, M.insert (Position row col) (Left', 0) c)
      | elem s ['>'] = (M.insert (Position row col) Horizontal g, M.insert (Position row col) (Right', 0) c)
    segments _ _ = error "segments: Unexpected pattern match."

-- | read the input file
input1 :: String
input1 = inputRaw1 "input/Day13input.txt"

-- | the parsed input.
parsedInput :: (Grid, Carts)
parsedInput = inputParser parseInit "input/Day13input.txt"

parseInit :: Parser (Grid, Carts)
parseInit = toGrid <$> manyTill (parseLine <* optional newline) eof where
  toGrid lines' = (M.fromList grid, M.fromList carts) where
    lines'' = concat lines'
    grid = map fst lines''
    carts = mapMaybe snd lines''

parseLine :: Parser [(Grid', Maybe Carts')]
parseLine = space *> many (parseHorizontal <|> parseVertical <|> parseSlash <|> parseBackSlash <|> parseIntersection <|> parseCartUp <|> parseCartDown <|> parseCartLeft <|> parseCartRight) <* space

parseHorizontal, parseVertical, parseSlash, parseBackSlash, parseIntersection, parseCartUp, parseCartDown, parseCartLeft, parseCartRight :: Parser (Grid', Maybe Carts')
parseHorizontal = toHorizontal <$> getSourcePos <* char '-' where
  toHorizontal sp = ((toPosition sp, Horizontal), Nothing)
parseVertical = toVertical <$> getSourcePos <* char '|' where
  toVertical sp = ((toPosition sp, Vertical), Nothing)
parseSlash = toTurnSlash <$> getSourcePos <* char '/' where
  toTurnSlash sp = ((toPosition sp, TurnSlash), Nothing)
parseBackSlash = toTurnBackSlash <$> getSourcePos <* char '\\' where
  toTurnBackSlash sp = ((toPosition sp, TurnBackSlash), Nothing)
parseIntersection = toIntersection <$> getSourcePos <* char '+' where
  toIntersection sp = ((toPosition sp, Intersection), Nothing)
parseCartUp = toCartUp <$> getSourcePos <* char '^' where
  toCartUp sp = ((toPosition sp, Vertical), Just $ (toPosition sp, (Up', 0)))
parseCartDown = toCartDown <$> getSourcePos <* char 'v' where
  toCartDown sp = ((toPosition sp, Vertical), Just $ (toPosition sp, (Down', 0)))
parseCartLeft = toCartLeft <$> getSourcePos <* char '<' where
  toCartLeft sp = ((toPosition sp, Horizontal), Just $ (toPosition sp, (Left', 0)))
parseCartRight = toCartRight <$> getSourcePos <* char '>' where
  toCartRight sp = ((toPosition sp, Horizontal), Just $ (toPosition sp, (Right', 0)))

toPosition :: SourcePos -> Position
toPosition sp = Position row col where
  row = (unPos (sourceLine sp)) - 1
  col = (unPos (sourceColumn sp)) - 1

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
-- 4. before tick -<<-; during tick <-<-; after tick <<--; NO collision
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
    checkForCollison = M.member
    checkForCollison' = elem
    turn d p i = M.insert p (d, i)
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
      | checkForCollison' p cos = (cs, cos)
      | checkForCollison p' cs = (M.delete p' cs, p' : cos)
      | grid M.! p' == TurnSlash = (turn Right' p' i cs, cos)
      | grid M.! p' == TurnBackSlash = (turn Left' p' i cs, cos)
      | grid M.! p' == Intersection = (turn (d4i Up' i) p' (i + 1) cs, cos)
      | otherwise = (turn Up' p' i cs, cos)
      where
        p' = Position (row - 1) col
    move (cs, cos) p@(Position row col) (Down', i)
      | checkForCollison' p cos = (cs, cos)
      | checkForCollison p' carts = (cs, p' : cos)
      | grid M.! p' == TurnSlash = (turn Left' p' i cs, cos)
      | grid M.! p' == TurnBackSlash = (turn Right' p' i cs, cos)
      | grid M.! p' == Intersection = (turn (d4i Down' i) p' (i + 1) cs, cos)
      | otherwise = (turn Down' p' i cs, cos)
      where
        p' = Position (row + 1) col
    move (cs, cos) p@(Position row col) (Left', i)
      | checkForCollison' p cos = (cs, cos)
      | checkForCollison p' cs = (M.delete p' cs, p' : cos)
      | grid M.! p' == TurnSlash = (turn Down' p' i cs, cos)
      | grid M.! p' == TurnBackSlash = (turn Up' p' i cs, cos)
      | grid M.! p' == Intersection = (turn (d4i Left' i) p' (i + 1) cs, cos)
      | otherwise = (turn Left' p' i cs, cos)
      where
        p' = Position row (col - 1)
    move (cs, cos) p@(Position row col) (Right', i)
      | checkForCollison' p cos = (cs, cos)
      | checkForCollison p' carts = (cs, p' : cos)
      | grid M.! p' == TurnSlash = (turn Up' p' i cs, cos)
      | grid M.! p' == TurnBackSlash = (turn Down' p' i cs, cos)
      | grid M.! p' == Intersection = (turn (d4i Right' i) p' (i + 1) cs, cos)
      | otherwise = (turn Right' p' i cs, cos)
      where
        p' = Position row (col + 1)
