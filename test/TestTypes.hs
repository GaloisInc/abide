module TestTypes where

import           Data.Word

import           Abide.Types.Arch.X86_64
import           Abide.Types
import           Abide.CTypes

import qualified Data.Map.Strict as M

newtype FnParameters reg = FnParameters { params :: [(CType, Either reg StackOffset)] }
  deriving (Eq, Show)

-- A mapping from registers to their contents
type RegVals = M.Map X86_64Registers Word64

-- A mapping from argument values to their offset on the stack.  The arguments
-- are assumed to be unique, known values.
type StackVals = M.Map Word64 StackOffset

type FnParamSpec = [(CType, Word64)]
