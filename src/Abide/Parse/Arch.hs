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
import           Abide.Types.Arch.PPC as PPC
import           Abide.Types.Arch.X86_64 as X64

x64Registers :: Parser X86_64Registers
x64Registers =  MP.string "RDI" $> RDI
            <|> MP.string "RSI" $> RSI
            <|> MP.string "RDX" $> RDX
            <|> MP.string "RCX" $> RCX
            <|> MP.string "R8"  $> X64.R8
            <|> MP.string "R9"  $> X64.R9
            <|> MP.string "StackInt" $> StackInt
            <|> MP.string "MMX0" $> MMX0
            <|> MP.string "MMX1" $> MMX1
            <|> MP.string "MMX2" $> MMX2
            <|> MP.string "MMX3" $> MMX3
            <|> MP.string "MMX4" $> MMX4
            <|> MP.string "MMX5" $> MMX5
            <|> MP.string "MMX6" $> MMX6
            <|> MP.string "MMX7" $> MMX7
            <|> MP.string "StackFloat" $> X64.StackFloat
            <|> MP.string "StackMem" $> StackMem

ppcRegisters :: Parser PPCRegisters
ppcRegisters =  MP.string "R3"  $> PPC.R3
            <|> MP.string "R4"  $> PPC.R4
            <|> MP.string "R5"  $> PPC.R5
            <|> MP.string "R6"  $> PPC.R6
            <|> MP.string "R7"  $> PPC.R7
            <|> MP.string "R8"  $> PPC.R8
            <|> MP.string "R9"  $> PPC.R9
            <|> MP.string "R10" $> PPC.R10  -- match these before F1
            <|> MP.string "F10" $> PPC.F10
            <|> MP.string "F11" $> PPC.F11
            <|> MP.string "F12" $> PPC.F12
            <|> MP.string "F13" $> PPC.F13
            <|> MP.string "F1"  $> PPC.F1
            <|> MP.string "F2"  $> PPC.F2
            <|> MP.string "F3"  $> PPC.F3
            <|> MP.string "F4"  $> PPC.F4
            <|> MP.string "F5"  $> PPC.F5
            <|> MP.string "F6"  $> PPC.F6
            <|> MP.string "F7"  $> PPC.F7
            <|> MP.string "F8"  $> PPC.F8
            <|> MP.string "F9"  $> PPC.F9
            <|> MP.string "StackGP" $> PPC.StackGP
            <|> MP.string "StackFloat" $> PPC.StackFloat            