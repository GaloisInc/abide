{-# LANGUAGE OverloadedStrings #-}

module TestParser where

import           Control.Monad.Combinators ( (<|>), count, skipMany )
import           Data.Foldable ( foldrM )
import           Data.Functor ( ($>) )
import qualified Data.Map.Strict as M
import           Data.Maybe ( catMaybes )
import qualified Data.Text as T
import           Data.Word
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL

import           Abide.CTypes
import           Abide.Types
import           Abide.Types.Arch.X86_64

import           TestTypes

type Parser = MP.Parsec T.Text T.Text

-- The entry point for parsing LLDB dumps.  First we filter down to those
-- lines which contain register values, and then we parse them all.
parseDump :: [T.Text] -> [(CType, Word64)] -> (RegVals, StackVals)
parseDump t params = (parseRegs t, parseStack t params)

parseRegs :: [T.Text] -> RegVals
parseRegs t = foldr (parseAndInsert parseOneReg) M.empty (filter isRegLine t)

parseStack :: [T.Text] -> [(CType, Word64)] -> StackVals
parseStack t params =
  foldr (parseListAndInsert (parseOneStackLine rspAddr params))
        M.empty
        (filter isStackLine t)
    where
      rspAddr = parseRSPAddr (head $ filter isRSPLine t)
      isRSPLine txt = T.isPrefixOf "rsp" (T.strip txt)

-- Check whether a line is a register value mapping that we care about, as
-- they all start with the name of the register.
isRegLine :: T.Text -> Bool
isRegLine txt = any (`T.isPrefixOf` (T.strip txt)) regStrs

isStackLine :: T.Text -> Bool
isStackLine txt = T.isPrefixOf "0x" (T.strip txt)

parseRSPAddr :: T.Text -> Word64
parseRSPAddr txt =
  case MP.parse parseOneReg "" txt of
    Right (RSP, w) -> w
    Left _ -> error "failed parsing RSP, which shouldn't happen"

parseAndInsert :: Ord k => Parser (k, v) -> T.Text -> M.Map k v -> M.Map k v
parseAndInsert p line map =
  case MP.parse p "" line of
    Right (k, v) -> M.insert k v map
    Left _ -> map

-- Run the actual parser for a particular register and insert it into the
-- mapping.
parseListAndInsert :: Ord k => Parser [(k, v)] -> T.Text -> M.Map k v -> M.Map k v
parseListAndInsert p line map =
  case MP.parse p "" line of
    Right kvs -> foldr (\(k,v) m -> M.insert k v m) map kvs
    Left _ -> map

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

-- The stack dump looks like "0x7fffffffdd58: 11 00 00 ................"
-- There are 16 hex bytes per line followed by 16 dots/characters for ascii
parseOneStackLine :: Word64 -> [(CType, Word64)] -> Parser [(Word64, StackOffset)]
parseOneStackLine rspAddr params = do
  startAddr <- parseOneHex
  symbol ":"
  bytes <- count 16 MPL.hexadecimal
  -- skipMany
  return $ matchBytesWithParams (rspAddr - startAddr) params bytes

matchBytesWithParams :: Word64 -> [(CType, Word64)] -> [Word64] -> [(Word64, StackOffset)]
matchBytesWithParams addr params bytes = catMaybes $ map (findOneParam addr bytes) params

findOneParam :: Word64 -> [Word64] -> (CType, Word64) -> Maybe (Word64, StackOffset)
findOneParam _ [] _ = Nothing
findOneParam addr (b:bs) (ct, val) =
  let trailingZs = fromIntegral . pred $ ctypeByteSize ct
  in if b == val && length bs >= trailingZs && all (== 0) (take trailingZs bs)
     then Just (val, undefined)
     else findOneParam addr bs (ct, val)



--------------------------------------------------------------------------------
-- Some parsing helpers

symbol = MPL.symbol MPC.space

regStrs = [ "rdi ", "rsi ", "rdx ", "rcx ", "r8 ", "r9 "
          , "ymm0 ", "ymm1 ", "ymm2 ", "ymm3 ", "ymm4 ", "ymm5 ", "ymm6 ", "ymm7 " ]
