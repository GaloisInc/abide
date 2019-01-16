{-# LANGUAGE MultiParamTypeClasses #-}

module TestTypes where

import qualified Data.Map.Strict as M
import qualified Text.Megaparsec as MP
import qualified Data.Text as T
import           Data.Word

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
