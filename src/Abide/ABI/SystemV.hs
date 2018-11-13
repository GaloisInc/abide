module Abide.ABI.SystemV where

import qualified Data.Map.Strict as M
import qualified Data.Universe as U

import           Abide.Types
import           Abide.Types.ABI.SystemV
import qualified Abide.Types.Arch.PPC32 as PPC32
import qualified Abide.Types.Arch.PPC64 as PPC64
import qualified Abide.Types.Arch.X86_64 as X64

{-|
Module : Abide.ABI.SystemV
Copyright : (c) 2018 Galois Inc.
License : BSD3 (see LICENSE)

This module defines the SystemV specific register sequences which are used to
generate the FSTs.  More architectures coming soon.
-}


x86IntRegs = [X64.RDI, X64.RSI, X64.RDX, X64.RCX, X64.R8, X64.R9, X64.StackInt]

x86FloatRegs = [X64.YMM0, X64.YMM1, X64.YMM2, X64.YMM3, X64.YMM4, X64.YMM5, X64.YMM6, X64.YMM7, X64.StackFloat]

x86MemRegs = [X64.StackMem]

x86_64Typemap :: TypeMap X86_64Classes X64.X86_64Registers
x86_64Typemap = M.fromList
  [ (INTEGER, x86IntRegs )
  , (SSE,     x86FloatRegs   )
  , (MEMORY,  x86MemRegs  )]

x86fp = "x86_64"

ppc32GPRegs = [ PPC32.R3, PPC32.R4, PPC32.R5, PPC32.R6, PPC32.R7
              , PPC32.R8, PPC32.R9, PPC32.R10, PPC32.StackGP]

ppc32FloatRegs = [ PPC32.F1, PPC32.F2, PPC32.F3, PPC32.F4, PPC32.F5, PPC32.F6
                 , PPC32.F7, PPC32.F8, PPC32.F9, PPC32.F10, PPC32.F11, PPC32.F12
                 , PPC32.F13, PPC32.StackFloat ]

ppc64GPRegs = [ PPC64.R3, PPC64.R4, PPC64.R5, PPC64.R6, PPC64.R7
              , PPC64.R8, PPC64.R9, PPC64.R10, PPC64.StackGP]

ppc64FloatRegs = [ PPC64.F1, PPC64.F2, PPC64.F3, PPC64.F4, PPC64.F5, PPC64.F6
                 , PPC64.F7, PPC64.F8, PPC64.F9, PPC64.F10, PPC64.F11, PPC64.F12
                 , PPC64.F13, PPC64.StackFloat ]

ppc32Typemap :: TypeMap PPC32Classes PPC32.PPC32Registers
ppc32Typemap = M.fromList
  [ (PPC32GP, ppc32GPRegs)
  , (PPC32FLOAT, ppc32FloatRegs)
  ]

ppc64Typemap :: TypeMap PPC64Classes PPC64.PPC64Registers
ppc64Typemap = M.fromList
  [ (PPC64GP, ppc64GPRegs)
  , (PPC64FLOAT, ppc64FloatRegs)
  ]
