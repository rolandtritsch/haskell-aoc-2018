{-|
Problem: <https://adventofcode.com/2018/day/16>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day16 where

import Text.Megaparsec (sepBy, between, many, eof, optional)
import Text.Megaparsec.Char (string, space1, newline)
import Text.Megaparsec.Char.Lexer (decimal)

import Util (inputRaw1, inputParser, Parser, signedInteger)

data Instruction = Instruction {
  iOpCode :: Int,
  iOperandA :: Int,
  iOperandB :: Int,
  iTarget :: Int
  } deriving (Show, Eq)

type Registers = [Int]
data Observation = Observation Registers Instruction Registers deriving (Show, Eq)
type Observations = [Observation]
type Instructions = [Instruction]

-- | the input as a string (for testing the parser).
input11, input12 :: String
input11 = inputRaw1 "input/Day16input1.txt"
input12 = inputRaw1 "input/Day16input2.txt"

-- | the parsed input for Part 1.
parsedInput1 :: Observations
parsedInput1 = inputParser parseObservations "input/Day16input1.txt"

-- | the parsed input for Part 2.
parsedInput2 :: Instructions
parsedInput2 = inputParser parseInstructions "input/Day16input2.txt"

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
parseObservations = many (parseObservation <* newline <* optional newline) <* eof

-- | parse the instrcutions.
parseInstructions :: Parser Instructions
parseInstructions = many (parseInstruction <* optional newline) <* eof
