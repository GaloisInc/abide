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
  = RAX
  | RBX
  | RCX
  | RDX
  | RSI
  | RDI
  | RBP
  | RSP
  | R8
  | R9
  | YMM0
  | YMM1
  | YMM2
  | YMM3
  | YMM4
  | YMM5
  | YMM6
  | YMM7
  | StackInt
  | StackFloat
  | StackMem
  deriving (Bounded, Enum, Eq, Ord, Show)

instance U.Universe X86_64Registers
