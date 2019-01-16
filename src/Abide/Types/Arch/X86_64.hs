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
  | XMM0
  | XMM1
  | XMM2
  | XMM3
  | XMM4
  | XMM5
  | XMM6
  | XMM7
  | StackInt
  | StackFloat
  | StackMem
  deriving (Bounded, Enum, Eq, Ord, Show)

instance U.Universe X86_64Registers
