{-|
Module : Abide.Types.ABI.SystemV
Copyright : (c) 2018 Galois Inc.
License : BSD3 (see LICENSE)

SystemV specific types.
-}
module Abide.Types.ABI.SystemV where

import qualified Data.Universe as U

data X86_64Classes
  = INTEGER
  | SSE
  | SSEUP
  | X87
  | X87UP
  | COMPLEX_X87
  | NO_CLASS
  | MEMORY
  deriving (Bounded, Enum, Eq, Ord, Show)

data PPCClasses = PPCGP | PPCFLOAT
  deriving (Bounded, Enum, Eq, Ord, Show)

instance U.Universe PPCClasses
instance U.Universe X86_64Classes

