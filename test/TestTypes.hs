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

-- Some compilers put parameters on the stack that are also in registers.  For
-- this reason, we need to track which parameters we've already found in
-- registers and avoid accidentally finding those on the stack.  Hence the
-- flag.
data FFlag = Found | NotFound deriving (Eq, Show)

type FnParamSpec = [(CType, Word64)]

type FlaggedParams = [(CType, Word64, FFlag)]
