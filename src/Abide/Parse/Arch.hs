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
import           Abide.Types.Arch.X86_64

x64Registers :: Parser X86_64Registers
x64Registers =  MP.string "RDI" $> RDI
            <|> MP.string "RSI" $> RSI
            <|> MP.string "RDX" $> RDX
            <|> MP.string "RCX" $> RCX
            <|> MP.string "R8"  $> R8
            <|> MP.string "R9"  $> R9
            <|> MP.string "StackInt" $> StackInt
            <|> MP.string "MMX0" $> MMX0
            <|> MP.string "MMX1" $> MMX1
            <|> MP.string "MMX2" $> MMX2
            <|> MP.string "MMX3" $> MMX3
            <|> MP.string "MMX4" $> MMX4
            <|> MP.string "MMX5" $> MMX5
            <|> MP.string "MMX6" $> MMX6
            <|> MP.string "MMX7" $> MMX7
            <|> MP.string "StackFloat" $> StackFloat
            <|> MP.string "StackMem" $> StackMem