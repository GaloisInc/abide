{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TestParser where


import           Control.Monad.Combinators ( (<|>), count, sepBy, skipMany )
import           Data.Foldable ( foldrM )
import           Data.Functor ( ($>) )
import           Data.List ( break, find )
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

karl :: Show a => Parser a -> T.Text -> IO ()
karl p t = case MP.parse p "" t of
  Right x -> print x
  Left e -> error (show e)

-- | The main entry point for parsing a dump from one of the generated C
-- programs.
parseCout :: FnParamSpec -> T.Text -> (RegVals, StackVals)
parseCout ps txt =
  let ts = T.lines txt
  in (parseRegs ts, parseStack ts ps)

--------------------------------------------------------------------------------
-- Register parsing.

-- | Instantiate an empty register mapping, grab the relevant lines from the
-- debugger dump, and parse each of them.
parseRegs :: [T.Text] -> RegVals
parseRegs t =
  foldr (parseAndInsert parseOneReg) M.empty (filter isRegLine t)

-- | Check whether a line is a register value mapping that we care about, as
-- they all start with the name of the register.
isRegLine :: T.Text -> Bool
isRegLine txt = any (`T.isPrefixOf` T.strip txt) regStrs

-- | The parser for a register value mapping.
parseOneReg :: Parser (X86_64Registers, Word64)
parseOneReg = do
  regName <- parseRegName
  symbol ":"
  (regName, ) <$> MPL.hexadecimal

-- -- | Parse the register names we care about.
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
-- Stack parsing stuff

-- | Instantiate an empty stack offset mapping, grab the relevant lines from
-- the debugger dump, and parse them.  This also requires the value of RSP in
-- order to compute the proper offset.
parseStack :: [T.Text] -> FnParamSpec -> StackVals
parseStack t ps =
  let txt = filter isStackLine t
      ts = divideByDiv txt
      res = zipWith runStackParser ts ps
  in foldr insertIfFound M.empty res

-- | For each parameter we look up, we may or may not compute a stack offset
-- for it.  If we do, insert it into a map.
insertIfFound :: Ord k => (a, k, Maybe v) -> M.Map k v -> M.Map k v
insertIfFound (_, k, Just v) = M.insert k v
insertIfFound _ = id

-- | The stack dump is inefficient and dumps the whole stack for each
-- parameter, because doing that for certain byte widths is easier than trying
-- to stitch together bytes into larger parameters.  Each dump is separated by
-- a line of text.  This function breaks the text according to that dividing
-- text, so each result list is appropriate for one parameter.
divideByDiv :: [T.Text] -> [[T.Text]]
divideByDiv ts =
  case break (T.isPrefixOf "parameter div") ts of
    (p,[])    -> [p]
    (p, rest) -> p : divideByDiv (drop 1 rest)


-- | Determine whether a line is part of the stack memory dump, all of which
-- start with a hex address.
isStackLine :: T.Text -> Bool
isStackLine txt = T.isPrefixOf "offset" txt
               || T.isPrefixOf "parameter div" txt

-- | Run the actual stack parser and dispatch on the result.
runStackParser :: [T.Text] -> (CType, Word64) -> (CType, Word64, Maybe StackOffset)
runStackParser txt (ct, w) =
  case MP.parse (parseOneStackParam w) "" (T.strip (T.unlines txt)) of
    Right (Just offset) -> (ct, w, Just offset)
    Left _ -> error (show txt) -- (ct, w, Nothing)

-- | All the lines relevant to some particular stack parameter are separated
-- by newlines, so parse each portion separately.
parseOneStackParam :: Word64 -> Parser (Maybe StackOffset)
parseOneStackParam w = do
  xs <- sepBy parseOneStackLine MPC.newline
  case find ((== w) . fst) xs of
    Just (_, so) -> return (Just so)
    Nothing -> return Nothing

-- | One line of the stack dump looks like "offset <offset> <val>"
parseOneStackLine :: Parser (Word64, StackOffset)
parseOneStackLine = do
  symbol "offset"
  so <- MPL.decimal
  MPC.space
  w <- MPL.hexadecimal
  return (w, so)

--------------------------------------------------------------------------------
-- Some generally useful parsing helpers

-- | Run a parser producing a single key/value pair and insert it into a map.
parseAndInsert :: Ord k => Parser (k, v) -> T.Text -> M.Map k v -> M.Map k v
parseAndInsert p line map = case MP.parse p "" line of
  Right (k, v) -> M.insert k v map
  Left _ -> map

symbol = MPL.symbol MPC.space

regStrs = [ "rdi ", "rsi ", "rdx ", "rcx ", "r8 ", "r9 "
          , "ymm0 ", "ymm1 ", "ymm2 ", "ymm3 ", "ymm4 ", "ymm5 ", "ymm6 ", "ymm7 " ]
