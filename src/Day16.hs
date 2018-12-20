{-|
Problem: <https://adventofcode.com/2018/day/16>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day16 where

import Text.Megaparsec (sepBy, between, many, eof)
import Text.Megaparsec.Char (string, space1, newline)
import Text.Megaparsec.Char.Lexer (decimal)

import Util (inputRaw, Parser, signedInteger)

data Instruction = Instruction {
  iOpCode :: Int,
  iOperandA :: Int,
  iOperandB :: Int,
  iTarget :: Int
  } deriving (Show, Eq)

type Registers = [Int]
data Observation = Observation Registers Instruction Registers deriving (Show, Eq)
type Observations = [Observation]

-- | read the input file(s)
input1, input2 :: [String]
input1 = inputRaw "input/Day16input1.txt"
input2 = inputRaw "input/Day16input2.txt"

-- | parse some a set of registers "[9, 10, -1]"
parseRegisters :: Parser Registers
parseRegisters = between (string "[") (string "]") (sepBy signedInteger (string ", "))

-- | parse an instruction "1 2 3 4"
parseInstruction :: Parser Instruction
parseInstruction = Instruction
  <$> decimal <* space1
  <*> decimal <* space1
  <*> decimal <* space1
  <*> decimal

-- | parse an observation.
parseObservation :: Parser Observation
parseObservation = Observation
  <$ string "Before: "
  <*> parseRegisters
  <* newline
  <*> parseInstruction
  <* newline
  <* string "After:  "
  <*> parseRegisters

-- | parse the observations.
parseObservations :: Parser Observations
parseObservations = many (parseObservation <* newline <* newline) <* eof
