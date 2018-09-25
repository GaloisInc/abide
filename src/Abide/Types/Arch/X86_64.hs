{-|
Module : Abide.Types.Arch.X86_64
Copyright : (c) 2018 Galois Inc.
License : BSD3 (see LICENSE)

This defines parsers specific to X86_64
-}
module Abide.Types.Arch.X86_64 where

import qualified Data.Universe as U

-- This module just defines the various registers that are interesting.
-- Currently a bit of a hack with the various Stack constructors, maybe use
-- something Either-like instead?
data X86_64Registers
  = RDI | RSI | RDX | RCX | R8 | R9 | StackInt
  | MMX0 | MMX1 | MMX2 | MMX3 | MMX4 | MMX5 | MMX6 | MMX7 | StackFloat
  | StackMem
  deriving (Bounded, Enum, Eq, Ord, Show)

instance U.Universe X86_64Registers
