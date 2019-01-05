{-# LANGUAGE OverloadedStrings #-}

module TestParser where

import           Control.Monad.Combinators ( (<|>), count, skipMany )
import           Data.Foldable ( foldrM )
import           Data.Functor ( ($>) )
import qualified Data.Map.Strict as M
import           Data.Maybe ( catMaybes )
import qualified Data.Text as T
import           Data.Word
import           Numeric.Natural
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL

import           Abide.CTypes
import           Abide.Types
import           Abide.Types.Arch.X86_64

import           TestParams
import           TestTypes

type Parser = MP.Parsec T.Text T.Text

-- | The entry point for parsing LLDB dumps.  First we filter down to those
-- lines which contain register values, and then we parse them all.
parseDump :: [T.Text] -> FnParamSpec -> (RegVals, StackVals)
parseDump t params =
  let rvs = parseRegs t
      fParams = flagParams rvs params
  in (rvs, parseStack t fParams)

-- | Assign flags to the parameter list indicating which were found in
-- registers.
flagParams :: RegVals -> FnParamSpec -> FlaggedParams
flagParams rvs ps = map (rvElem rvs) ps
  where
    rvElem :: RegVals -> (CType, Word64) -> (CType, Word64, FFlag)
    rvElem rvs (t, w) = (t, w, if elem w (M.elems rvs) then Found else NotFound)

--------------------------------------------------------------------------------
-- Register parsing.  The lines of the dump corresponding to registers are
-- easy to parse.  They all start with the name of a single register followed
-- by the bytes stored there.  More details are in some of the documentation
-- for specific parsing functions below.

-- | Instantiate an empty register mapping, grab the relevant lines from the
-- debugger dump, and parse each of them.
parseRegs :: [T.Text] -> RegVals
parseRegs t = foldr (parseAndInsert parseOneReg) M.empty (filter isRegLine t)

-- | Check whether a line is a register value mapping that we care about, as
-- they all start with the name of the register.
isRegLine :: T.Text -> Bool
isRegLine txt = any (`T.isPrefixOf` (T.strip txt)) regStrs

-- | While parameters are not ever in RBP, we still need to find it when we
-- are computing offsets into the stack.
parseRBPAddr :: T.Text -> Word64
parseRBPAddr txt = case MP.parse parseOneReg "" txt of
  Right (RBP, w) -> w
  Left e -> error (show e)

-- | Run a parser producing a single key/value pair and insert it into a map.
-- This is relevant only to registers for now, since the stack stuff produces
-- lists of results.
parseAndInsert :: Ord k => Parser (k, v) -> T.Text -> M.Map k v -> M.Map k v
parseAndInsert p line map = case MP.parse p "" line of
  Right (k, v) -> M.insert k v map
  Left _ -> map

-- | The parser for a register value mapping.  LLDB uses a different format
-- for the YMM registers than for the general purpose ones, so we need to
-- handle those separately.
parseOneReg :: Parser (X86_64Registers, Word64)
parseOneReg = do
  MPC.space
  regName <- parseRegName
  symbol "="
  if isFPReg regName
    then parseManyBytes >>= \w -> return (regName, w)
    else parseHex >>= \w -> return (regName, w)

-- | The YMM registers look like: "YMM0 = {0x00 0x00 .. }"
parseManyBytes :: Parser Word64
parseManyBytes = do
  symbol "{"
  -- For now, we only care about the first byte.  Eventually tests may need to
  -- grow, and this will change.
  byte <- parseHex
  MP.many parseHex
  symbol "}"
  return byte

-- | Parse a single hex value with the typical '0x' prefix
parseHex :: Parser Word64
parseHex = MPC.char '0' >> MPC.char' 'x' >> parseHexNoPref

-- | Parse a hex value without a '0x' prefix
parseHexNoPref :: Parser Word64
parseHexNoPref = do
  x <- MPL.hexadecimal
  MPC.space
  return x

-- | Parse the register names we care about.
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
            <|> symbol "rbp" $> RBP

--------------------------------------------------------------------------------
-- Stack parsing stuff.  This is a bit more complicated than the register
-- stuff, because we have to compute the offsets here, where we know the value
-- of RSP, and the addresses where the magic values are found on the stack.

-- | Instantiate an empty stack offset mapping, grab the relevant lines from
-- the debugger dump, and parse them.  This also requires the value of RSP in
-- order to compute the proper offset.
parseStack :: [T.Text] -> FlaggedParams -> StackVals
parseStack t params =
  foldr (parseListAndInsert (parseOneStackLine rbpAddr params)) M.empty (filter isStackLine t)
    where
      rbpAddr = fromIntegral $ parseRBPAddr (head $ filter isRBPLine t)
      isRBPLine txt = T.isPrefixOf "rbp" (T.strip txt)

-- | Run a parser producing a list of values.  This is only relevant to the
-- stack for now, since a single line from the dump can contain multiple
-- parameters.
parseListAndInsert :: Ord k => Parser [(k, v)] -> T.Text -> M.Map k v -> M.Map k v
parseListAndInsert p line map =
  case MP.parse p "" line of
    Right kvs -> foldr (\(k,v) m -> M.insert k v m) map kvs
    Left e -> error (show e)

-- | Determine whether a line is part of the stack memory dump, all of which
-- start with a hex address.
isStackLine :: T.Text -> Bool
isStackLine txt = T.isPrefixOf "0x" (T.strip txt)

-- | The stack dump looks like "0x7fffffffdd58: 11 00 00 ................"
-- There are 16 hex bytes per line followed by 16 dots/characters for ascii
parseOneStackLine
  :: Natural
  -- ^ The value of the RBP register
  -> FlaggedParams
  -> Parser [(Word64, StackOffset)]
parseOneStackLine rbpAddr params = do
  startAddr <- parseHex
  symbol ":"
  bytes <- count 16 parseHexNoPref
  return $ matchBytesWithParams rbpAddr (fromIntegral startAddr) params (map fromIntegral bytes)

matchBytesWithParams
  :: Natural
  -- ^ The value of the RBP register
  -> Natural
  -- ^ The address of the bytes we are inspecting
  -> FlaggedParams
  -> [Word8]
  -- ^ The 16 bytes for a single line of the dump
  -> [(Word64, StackOffset)]
matchBytesWithParams rbp addr params bytes =
  catMaybes $ map (findOneParam (addr - rbp) bytes) params

-- | Try to find a particular parameter in the bytes from a single line of the
-- dump.  The parameters shouldn't cross the line boundary.
findOneParam
  :: Natural
  -- ^ The base offset for the line of the dump we are examining.  As we
  -- traverse it, we will increment this.
  -> [Word8]
  -- ^ The bytes to search
  -> (CType, Word64, FFlag)
  -> Maybe (Word64, StackOffset)
findOneParam offset (b:bs) (ct, val, NotFound) =
  let trailingZs = fromIntegral . pred $ ctypeByteSize ct
  in if fromIntegral b == val && length bs >= trailingZs && all (== 0) (take trailingZs bs)
     then Just (val, offset)
     else findOneParam (offset + 1) bs (ct, val, NotFound)
findOneParam _ _ _ = Nothing

--------------------------------------------------------------------------------
-- Some parsing helpers

symbol = MPL.symbol MPC.space

regStrs = [ "rdi ", "rsi ", "rdx ", "rcx ", "r8 ", "r9 "
          , "ymm0 ", "ymm1 ", "ymm2 ", "ymm3 ", "ymm4 ", "ymm5 ", "ymm6 ", "ymm7 " ]
