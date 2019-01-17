{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module TestTypes where

import qualified Data.Map.Strict as M
import qualified Text.Megaparsec as MP
import qualified Data.Text as T
import           Data.Word
import qualified System.FilePath as FP

import qualified Abide.Parse.Arch as AP
import           Abide.Types.Arch.X86_64
import           Abide.Types
import           Abide.CTypes


newtype FnParameters reg = FnParameters { params :: [(CType, Either reg StackOffset)] }
  deriving (Eq, Show)

-- A mapping from registers to their contents
type RegVals reg = M.Map Word64 reg

-- A mapping from argument values to their offset on the stack.  The arguments
-- are assumed to be unique, known values.
type StackVals = M.Map Word64 StackOffset

type FnParamSpec = [(CType, Word64)]

type Parser = MP.Parsec T.Text T.Text

-- | The type class that gives us the architecture-specific functionality.
class TestableArch arch abi where
  regParser :: Parser (OutSymbol arch abi)
  regStrings :: proxy (arch, abi) -> [T.Text]
  regVarNames :: proxy (arch, abi) -> [(OutSymbol arch abi, T.Text)]
  gccFP :: proxy (arch, abi) -> FP.FilePath

instance TestableArch X86_64 SystemV where
  regParser = AP.x64Registers
  regStrings _ = x64RegStrs
  regVarNames _ = x64RegVariables
  gccFP _ = "gcc"

x64RegStrs :: [T.Text]
x64RegStrs = [ "RDI", "RSI", "RDX", "RCX", "R8", "R9"
             , "XMM0", "XMM1", "XMM2", "XMM3", "XMM4", "XMM5", "XMM6", "XMM7" ]

ppcRegStrs :: [T.Text]
ppcRegStrs = [ "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10"
             , "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12", "F13" ]

x64RegVariables :: [(X86_64Registers, T.Text)]
x64RegVariables =
  let regs = [ RDI , RSI , RDX , RCX , R8 , R9
             , XMM0 , XMM1 , XMM2 , XMM3 , XMM4 , XMM5 , XMM6 , XMM7]
  in zip regs x64RegStrs
