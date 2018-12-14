{-# LANGUAGE OverloadedStrings #-}

module TestParser where

import           Control.Monad.Combinators ( (<|>) )
import           Data.Foldable ( foldrM )
import           Data.Functor ( ($>) )
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Word
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL

import           Abide.Types
import           Abide.Types.Arch.X86_64

import           TestTypes

type Parser = MP.Parsec T.Text T.Text

-- The entry point for parsing LLDB dumps.  First we filter down to those
-- lines which contain register values, and then we parse them all.
parseRegs :: [T.Text] -> RegVals
parseRegs tts = foldr parseAndInsert M.empty (filter isReg tts)

-- Check whether a line is a register value mapping that we care about, as
-- they all start with the name of the register.
isReg :: T.Text -> Bool
isReg txt = any (`T.isPrefixOf` (T.strip txt)) regStrs

-- Run the actual parser for a particular register and insert it into the
-- mapping.
parseAndInsert :: T.Text -> RegVals -> RegVals
parseAndInsert line rvs = do
  case MP.parse parseOneReg "" line of
    Right (k, v) -> M.insert k v rvs
    Left _ -> M.empty

-- The parser for a register value mapping.  LLDB uses a different format for
-- the YMM registers than for the general purpose ones, so we need to handle
-- those separately.
parseOneReg :: Parser (X86_64Registers, Word64)
parseOneReg = do
  MPC.space
  regName <- parseRegName
  symbol "="
  if isFPReg regName
    then parseManyBytes >>= \w -> return (regName, w)
    else parseOneHex >>= \w -> return (regName, w)

-- The YMM registers look like: "YMM0 = {0x00 0x00 .. }"
parseManyBytes :: Parser Word64
parseManyBytes = do
  symbol "{"
  byte <- parseOneHex
  MP.many parseOneHex
  symbol "}"
  return byte

-- Parse a single hex value and trailing space.
parseOneHex :: Parser Word64
parseOneHex = do
  MPC.char '0' >> MPC.char' 'x'
  x <- MPL.hexadecimal
  MPC.space
  return x

-- Parse the register names we care about.
parseRegName :: Parser X86_64Registers
parseRegName =  symbol "rdi" $> RDI
            <|> symbol "rsi" $> RSI
            <|> symbol "rdx" $> RDX
            <|> symbol "rcx" $> RCX
            <|> symbol "r8"  $> R8
            <|> symbol "r9"  $> R9
            <|> symbol "ymm0" $> YMM0
            <|> symbol "ymm1" $> YMM1
            <|> symbol "ymm2" $> YMM2
            <|> symbol "ymm3" $> YMM3
            <|> symbol "ymm4" $> YMM4
            <|> symbol "ymm5" $> YMM5
            <|> symbol "ymm6" $> YMM6
            <|> symbol "ymm7" $> YMM7

--------------------------------------------------------------------------------
-- Some parsing helpers

symbol = MPL.symbol MPC.space

regStrs = [ "rdi ", "rsi ", "rdx ", "rcx ", "r8 ", "r9 "
          , "ymm0 ", "ymm1 ", "ymm2 ", "ymm3 ", "ymm4 ", "ymm5 ", "ymm6 ", "ymm7 " ]
