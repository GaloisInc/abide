{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TestTypes where

import qualified Data.Map.Strict as M
import qualified Text.Megaparsec as MP
import qualified Data.Text as T
import           Data.Word
import qualified Language.C.Quote as C
import qualified System.FilePath as FP
import qualified Text.Megaparsec as MP

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

type Bytes = Int

-- | The type class that gives us the architecture-specific functionality.
class (Show (OutSymbol arch abi)) => TestableArch arch abi where
  regSize :: proxy (arch, abi) -> CType
  numRegs :: proxy (arch, abi) -> Int
  mkAsmHeader :: proxy (arch, abi) -> [T.Text]
  mkAsmFooter :: proxy (arch, abi) -> [T.Text]
  mkRegAsm :: proxy (arch, abi) -> (OutSymbol arch abi, T.Text) -> Int -> T.Text
  mkMemAsm :: proxy (arch, abi) -> Int -> [T.Text]
  regStrings :: proxy (arch, abi) -> [T.Text]
  regVarNames :: proxy (arch, abi) -> [(OutSymbol arch abi, T.Text)]
  ccFP :: proxy (arch, abi) -> FP.FilePath
  ccFlags :: proxy (arch, abi) -> [String]
  exeWrapper :: proxy (arch, abi) -> FP.FilePath -> (FP.FilePath, [String])
  regParser :: Parser (OutSymbol arch abi)
