{-# LANGUAGE OverloadedStrings #-}

{-|
Module : Abide.Parse.ABI
Copyright : (c) 2018 Galois Inc.
License : BSD3 (see LICENSE)

This defines parsers specific to particular architectures.
-}
module Abide.Parse.Arch where

import           Control.Monad ( msum )
import           Control.Monad.Combinators ( (<|>) )
import           Data.Functor ( ($>) )
import qualified Data.Text as T

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

import           Abide.Parse.Common
import           Abide.Types.Arch.PPC32 as PPC32
import           Abide.Types.Arch.PPC64 as PPC64
import           Abide.Types.Arch.X86_64 as X64

x64Registers :: Parser X86_64Registers
x64Registers =  MP.string "RDI" $> RDI
            <|> MP.string "RSI" $> RSI
            <|> MP.string "RDX" $> RDX
            <|> MP.string "RCX" $> RCX
            <|> MP.string "R8"  $> X64.R8
            <|> MP.string "R9"  $> X64.R9
            <|> MP.string "StackInt" $> StackInt
            <|> MP.string "XMM0" $> XMM0
            <|> MP.string "XMM1" $> XMM1
            <|> MP.string "XMM2" $> XMM2
            <|> MP.string "XMM3" $> XMM3
            <|> MP.string "XMM4" $> XMM4
            <|> MP.string "XMM5" $> XMM5
            <|> MP.string "XMM6" $> XMM6
            <|> MP.string "XMM7" $> XMM7
            <|> MP.string "StackFloat" $> X64.StackFloat
            <|> MP.string "StackMem" $> StackMem

ppc32Registers :: Parser PPC32Registers
ppc32Registers =  MP.string "R3"  $> PPC32.R3
              <|> MP.string "R4"  $> PPC32.R4
              <|> MP.string "R5"  $> PPC32.R5
              <|> MP.string "R6"  $> PPC32.R6
              <|> MP.string "R7"  $> PPC32.R7
              <|> MP.string "R8"  $> PPC32.R8
              <|> MP.string "R9"  $> PPC32.R9
              <|> MP.string "R10" $> PPC32.R10
              <|> MP.string "F10" $> PPC32.F10   -- match these before F1
              <|> MP.string "F11" $> PPC32.F11
              <|> MP.string "F12" $> PPC32.F12
              <|> MP.string "F13" $> PPC32.F13
              <|> MP.string "F1"  $> PPC32.F1
              <|> MP.string "F2"  $> PPC32.F2
              <|> MP.string "F3"  $> PPC32.F3
              <|> MP.string "F4"  $> PPC32.F4
              <|> MP.string "F5"  $> PPC32.F5
              <|> MP.string "F6"  $> PPC32.F6
              <|> MP.string "F7"  $> PPC32.F7
              <|> MP.string "F8"  $> PPC32.F8
              <|> MP.string "F9"  $> PPC32.F9
              <|> MP.string "StackGP" $> PPC32.StackGP
              <|> MP.string "StackFloat" $> PPC32.StackFloat

ppc64Registers :: Parser PPC64Registers
ppc64Registers =  MP.string "R3"  $> PPC64.R3
              <|> MP.string "R4"  $> PPC64.R4
              <|> MP.string "R5"  $> PPC64.R5
              <|> MP.string "R6"  $> PPC64.R6
              <|> MP.string "R7"  $> PPC64.R7
              <|> MP.string "R8"  $> PPC64.R8
              <|> MP.string "R9"  $> PPC64.R9
              <|> MP.string "R10" $> PPC64.R10
              <|> MP.string "F10" $> PPC64.F10   -- match these before F1
              <|> MP.string "F11" $> PPC64.F11
              <|> MP.string "F12" $> PPC64.F12
              <|> MP.string "F13" $> PPC64.F13
              <|> MP.string "F1"  $> PPC64.F1
              <|> MP.string "F2"  $> PPC64.F2
              <|> MP.string "F3"  $> PPC64.F3
              <|> MP.string "F4"  $> PPC64.F4
              <|> MP.string "F5"  $> PPC64.F5
              <|> MP.string "F6"  $> PPC64.F6
              <|> MP.string "F7"  $> PPC64.F7
              <|> MP.string "F8"  $> PPC64.F8
              <|> MP.string "F9"  $> PPC64.F9
              <|> MP.string "StackGP" $> PPC64.StackGP
              <|> MP.string "StackFloat" $> PPC64.StackFloat
