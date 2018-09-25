module Abide.ABI.SystemV where

import qualified Data.Map.Strict as M
import qualified Data.Universe as U

import           Abide.Types
import           Abide.Types.ABI.SystemV
import qualified Abide.Types.Arch.PPC as PPC
import qualified Abide.Types.Arch.X86_64 as X64

{-|
Module : Abide.ABI.SystemV
Copyright : (c) 2018 Galois Inc.
License : BSD3 (see LICENSE)

This module defines the SystemV specific register sequences which are used to
generate the FSTs.  More architectures coming soon.
-}


x86IntRegs = [X64.RDI, X64.RSI, X64.RDX, X64.RCX, X64.R8, X64.R9, X64.StackInt]

x86FloatRegs = [X64.MMX0, X64.MMX1, X64.MMX2, X64.MMX3, X64.MMX4, X64.MMX5, X64.MMX6, X64.MMX7, X64.StackFloat]

x86MemRegs = [X64.StackMem]

x86_64Typemap :: TypeMap X86_64Classes X64.X86_64Registers
x86_64Typemap = M.fromList
  [ (INTEGER, x86IntRegs )
  , (SSE,     x86FloatRegs   )
  , (MEMORY,  x86MemRegs  )]

x86fp = "x86_64"
