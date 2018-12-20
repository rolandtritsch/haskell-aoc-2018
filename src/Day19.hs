{-|
Problem: <https://adventofcode.com/2018/day/19>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day19 where

import Text.Megaparsec (manyTill, eof, (<|>), optional)
import Text.Megaparsec.Char (string, newline, space1)
import Text.Megaparsec.Char.Lexer (decimal)

import Util (inputRaw, inputRaw1, Parser)

type InstructionPointer = Int
data Register = Register Int deriving (Show, Eq)
type Value = Int
data Instruction
  = Addr Register Register Register
  | Addi Register Value Register
  | Mulr Register Register Register
  | Muli Register Value Register
  | Banr Register Register Register
  | Bani Register Value Register
  | Borr Register Register Register
  | Bori Register Value Register
  | Setr Register Register Register
  | Seti Register Value Register
  | Gtir Value Register Register
  | Gtri Register Value Register
  | Gtrr Register Register Register
  | Eqir Value Register Register
  | Eqri Register Value Register
  | Eqrr Register Register Register
  deriving (Show, Eq)
type Instructions = [Instruction]

-- | read the input file
input :: [String]
input = inputRaw "input/Day19input.txt"

-- | read the input file (as one line)
input1 :: String
input1 = inputRaw1 "input/Day19input.txt"

-- | parse the instruction pointer.
parseIp :: Parser Int
parseIp = string "#ip " *> decimal

-- | parse the program.
parseProgram :: Parser (InstructionPointer, Instructions)
parseProgram = (,)
  <$> parseIp <* newline
  <*> manyTill (parseInstruction <* optional newline) eof

-- | parse a register.
parseRegister :: Parser Register
parseRegister = Register <$> decimal

-- | parse a value.
parseValue :: Parser Int
parseValue = decimal

parseInstruction :: Parser Instruction
parseInstruction
  = parseAddr
  <|> parseAddi
  <|> parseMulr
  <|> parseMuli
  <|> parseBanr
  <|> parseBani
  <|> parseBorr
  <|> parseBori
  <|> parseSetr
  <|> parseSeti
  <|> parseGtir
  <|> parseGtri
  <|> parseGtrr
  <|> parseEqir
  <|> parseEqri
  <|> parseEqrr

parseAddr, parseAddi, parseMulr, parseMuli, parseBanr, parseBani, parseBorr, parseBori, parseSetr, parseSeti, parseGtir, parseGtri, parseGtrr, parseEqir, parseEqri, parseEqrr :: Parser Instruction
parseAddr = Addr <$ string "addr " <*> parseRegister <* space1 <*> parseRegister <* space1 <*> parseRegister
parseAddi = Addi <$ string "addi " <*> parseRegister <* space1 <*> parseValue <* space1 <*> parseRegister
parseMulr = Mulr <$ string "mulr " <*> parseRegister <* space1 <*> parseRegister <* space1 <*> parseRegister
parseMuli = Muli <$ string "muli " <*> parseRegister <* space1 <*> parseValue <* space1 <*> parseRegister
parseBanr = Banr <$ string "banr " <*> parseRegister <* space1 <*> parseRegister <* space1 <*> parseRegister
parseBani = Bani <$ string "bani " <*> parseRegister <* space1 <*> parseValue <* space1 <*> parseRegister
parseBorr = Borr <$ string "borr " <*> parseRegister <* space1 <*> parseRegister <* space1 <*> parseRegister
parseBori = Bori <$ string "bori " <*> parseRegister <* space1 <*> parseValue <* space1 <*> parseRegister
parseSetr = Setr <$ string "setr " <*> parseRegister <* space1 <*> parseRegister <* space1 <*> parseRegister
parseSeti = Seti <$ string "seti " <*> parseRegister <* space1 <*> parseValue <* space1 <*> parseRegister
parseGtir = Gtir <$ string "gtir " <*> parseValue <* space1 <*> parseRegister <* space1 <*> parseRegister
parseGtri = Gtri <$ string "gtri " <*> parseRegister <* space1 <*> parseValue <* space1 <*> parseRegister
parseGtrr = Gtrr <$ string "gtrr " <*> parseRegister <* space1 <*> parseRegister <* space1 <*> parseRegister
parseEqir = Eqir <$ string "eqir " <*> parseValue <* space1 <*> parseRegister <* space1 <*> parseRegister
parseEqri = Eqri <$ string "eqri " <*> parseRegister <* space1 <*> parseValue <* space1 <*> parseRegister
parseEqrr = Eqrr <$ string "eqrr " <*> parseRegister <* space1 <*> parseRegister <* space1 <*> parseRegister
