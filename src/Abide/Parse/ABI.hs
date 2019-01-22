{-# LANGUAGE OverloadedStrings #-}

{-|
Module : Abide.Parse.ABI
Copyright : (c) 2018 Galois Inc.
License : BSD3 (see LICENSE)

This defines parsers specific to particular ABIs.
-}

module Abide.Parse.ABI where

import           Control.Monad.Combinators ( (<|>) )
import           Data.Functor ( ($>) )

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

import           Abide.Parse.Common
import           Abide.Types
import qualified Abide.Types.ABI.SystemV as SV
import           Abide.Types.Arch.X86_64

x64Classes :: Parser SV.X86_64Classes
x64Classes =  MP.string "INTEGER"     $> SV.INTEGER
          <|> MP.string "SSE"         $> SV.SSE
          <|> MP.string "SSEUP"       $> SV.SSEUP
          <|> MP.string "X87"         $> SV.X87
          <|> MP.string "X87UP"       $> SV.X87UP
          <|> MP.string "COMPLEX_X87" $> SV.COMPLEX_X87
          <|> MP.string "NO_CLASS"    $> SV.NO_CLASS
          <|> MP.string "MEMORY"      $> SV.MEMORY

ppc32Classes :: Parser SV.PPC32Classes
ppc32Classes =  MP.string "PPC32GP"    $> SV.PPC32GP
            <|> MP.string "PPC32FLOAT" $> SV.PPC32FLOAT

ppc64Classes :: Parser SV.PPC64Classes
ppc64Classes =  MP.string "PPC64GP"    $> SV.PPC64GP
            <|> MP.string "PPC64FLOAT" $> SV.PPC64FLOAT
