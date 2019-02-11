{-|
Problem: <https://adventofcode.com/2018/day/10>

Solution:

General - Akward. Was guessing that I can find/detect
the text/message by looking for a col that is filled
with lights. Found out that there are more lights
than positions in the message (means some lights
are in the same position to form the message).

Part 1 - Return the message.

Part 2 - Return the number of secs it took to
find the message.
-}
module Day10 where

import Text.Megaparsec (many, eof, optional)
import Text.Megaparsec.Char (newline, string, char, space1)

import Util (inputRaw, inputRaw1, inputParser, Parser, signedInteger)

import Data.Maybe (isJust)
import Data.List (minimumBy, maximumBy, find)
import Data.List.Split (splitOneOf)

type Position = (Int, Int)
type Velocity = (Int, Int)
type Light = (Position, Velocity)
type Sky = [Light]

-- | read the input file
input :: Sky
input = (map process . inputRaw) "input/Day10input.txt" where
  process line = ((c,r), (a,b)) where
    -- position=< 9,  1> velocity=< 0,  2>
    tokens = filter (not . null) $ splitOneOf "=<>, " line
    c = read $ tokens !! 1
    r = read $ tokens !! 2
    a = read $ tokens !! 4
    b = read $ tokens !! 5

-- | read the input file
input1 :: String
input1 = inputRaw1 "input/Day10input.txt"

-- | the parsed input.
parsedInput :: Sky
parsedInput = inputParser parseTheSky "input/Day10input.txt"

parseTheSky :: Parser Sky
parseTheSky = many (parseLight <* optional newline) <* eof

parseLight :: Parser Light
parseLight = (,) <$> parsePosition <* space1 <*> parseVelocity

parsePosition :: Parser Position
parsePosition = (,)
  <$ string "position=<"
  <* optional (char ' ')
  <*> signedInteger
  <* string ", "
  <* optional (char ' ')
  <*> signedInteger
  <* char '>'

parseVelocity :: Parser Velocity
parseVelocity = (,)
  <$ string "velocity=<"
  <* optional (char ' ')
  <*> signedInteger
  <* string ", "
  <* optional (char ' ')
  <*> signedInteger
  <* char '>'

-- | move the sky by one second.
tick :: Sky -> Sky
tick = map update where
  update ((c, r), (a,b)) = ((c+a,r+b),(a,b))

-- | return the dimensions of the sky.
dimension :: Sky -> ((Int, Int),(Int, Int))
dimension s = ((minCol, maxCol), (minRow, maxRow)) where
  byCol ((c, _), _) ((c', _), _) = compare c c'
  byRow ((_, r), _) ((_, r'), _) = compare r r'
  (minCol, _) = fst $ minimumBy byCol s
  (maxCol, _) = fst $ maximumBy byCol s
  (_, minRow) = fst $ minimumBy byRow s
  (_, maxRow) = fst $ maximumBy byRow s

-- | moving through the night (until I see a full column of lights,
-- which makes me think that this is sky I am looking for).
-- Note: It seems the input has lights with different velocities
-- that will converge towards the message. As a matter of fact
-- there are 350 lights in the input and they converge into 181
-- positions that will form the message.
night :: Int -> Sky -> (Int, Sky)
night secs sky
  | iSeeSomething = (secs, sky)
  | otherwise = night (secs + 1) (tick sky)
  where
    ((minCol, maxCol),(minRow, maxRow)) = dimension sky
    iSeeSomething = any forVerticalLine [minCol..maxCol] where
      forVerticalLine c = (length . filter (\((c', _), _) -> c == c')) sky >= (maxRow - minRow + 1)

-- | paint the lights on the sky :).
paint :: Sky -> [String]
paint sky = map paintLine [minRow..maxRow] where
  ((minCol, maxCol),(minRow, maxRow)) = dimension sky
  paintLine r = foldl paintLight "" [minCol..maxCol] where
    paintLight line c
      | isJust (find lightOn sky) = line ++ "*"
      | otherwise = line ++ " "
      where
        lightOn ((c',r'),_) = c == c' && r == r'
