{-|
Module : Abide.Types.Arch.PPC64
Copyright : (c) 2018 Galois Inc.
License : BSD3 (see LICENSE)

PPC64 specific types
-}
module Abide.Types.Arch.PPC64 where

import qualified Data.Universe as U

data PPC64Registers
  = R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10
  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | F10
  | F11
  | F12
  | F13
  | V2
  | V3
  | V4
  | V5
  | V6
  | V7
  | V8
  | V9
  | V10
  | V11
  | V12
  | V13
  | StackGP
  | StackFloat
  | StackVec
  deriving (Bounded, Enum, Eq, Ord, Show)

instance U.Universe PPC64Registers
