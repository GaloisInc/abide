module TestTypes where

import           Data.Word

import           Abide.Types.Arch.X86_64
import           Abide.Types
import           Abide.CTypes

import qualified Data.Map.Strict as M

newtype FnParameters reg = FnParameters { params :: [(CType, Either reg StackOffset)] }
  deriving (Eq, Show)

type RegVals = M.Map X86_64Registers Word64

type StackVals = M.Map Word64 StackOffset
