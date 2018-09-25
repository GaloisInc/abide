{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module : Abide.Parse
Copyright : (c) 2018 Galois Inc.
License : BSD3 (see LICENSE)

This module defines a parser for the FST files.  It's important to generate
the files and parse them rather than implementing the computation directly
because it allows for external verification of the parameter passing rules.
-}

module Abide.Parse where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL


import Abide.Parse.ABI
import Abide.Parse.Arch
import Abide.Parse.Common
import Abide.Types
import Abide.Types.ABI.SystemV (X86_64Classes)
import Abide.Types.Arch.X86_64

x86_64FSTFromFile :: FilePath -> IO (Either (MP.ParseError Char T.Text) (FST X86_64 SystemV))
x86_64FSTFromFile fp = MP.parse (parseFST svClasses x64Registers) fp <$> T.readFile fp

--------------------------------------------------------------------------------
-- Below here is a big hack.  This is a temporary until we have a plan for how
-- to make the parsed FSTs available without having to recompute them
-- constantly.

x86_64FST :: FST X86_64 SystemV
x86_64FST = case MP.parse (parseFST svClasses x64Registers) "" x86_64FSTText of
  Right fst -> fst

x86_64FSTText :: T.Text
x86_64FSTText = T.unlines 
  [ "1 10 INTEGER RDI"
  , "1 2 SSE MMX0"
  , "1 1 MEMORY StackMem"
  , "2 11 INTEGER RDI"
  , "2 3 SSE MMX1"
  , "2 2 MEMORY StackMem"
  , "3 12 INTEGER RDI"
  , "3 4 SSE MMX2"
  , "3 3 MEMORY StackMem"
  , "4 13 INTEGER RDI"
  , "4 5 SSE MMX3"
  , "4 4 MEMORY StackMem"
  , "5 14 INTEGER RDI"
  , "5 6 SSE MMX4"
  , "5 5 MEMORY StackMem"
  , "6 15 INTEGER RDI"
  , "6 7 SSE MMX5"
  , "6 6 MEMORY StackMem"
  , "7 16 INTEGER RDI"
  , "7 8 SSE MMX6"
  , "7 7 MEMORY StackMem"
  , "8 17 INTEGER RDI"
  , "8 9 SSE MMX7"
  , "8 8 MEMORY StackMem"
  , "9 18 INTEGER RDI"
  , "9 9 SSE StackFloat"
  , "9 9 MEMORY StackMem"
  , "10 19 INTEGER RSI"
  , "10 11 SSE MMX0"
  , "10 10 MEMORY StackMem"
  , "11 20 INTEGER RSI"
  , "11 12 SSE MMX1"
  , "11 11 MEMORY StackMem"
  , "12 21 INTEGER RSI"
  , "12 13 SSE MMX2"
  , "12 12 MEMORY StackMem"
  , "13 22 INTEGER RSI"
  , "13 14 SSE MMX3"
  , "13 13 MEMORY StackMem"
  , "14 23 INTEGER RSI"
  , "14 15 SSE MMX4"
  , "14 14 MEMORY StackMem"
  , "15 24 INTEGER RSI"
  , "15 16 SSE MMX5"
  , "15 15 MEMORY StackMem"
  , "16 25 INTEGER RSI"
  , "16 17 SSE MMX6"
  , "16 16 MEMORY StackMem"
  , "17 26 INTEGER RSI"
  , "17 18 SSE MMX7"
  , "17 17 MEMORY StackMem"
  , "18 27 INTEGER RSI"
  , "18 18 SSE StackFloat"
  , "18 18 MEMORY StackMem"
  , "19 28 INTEGER RDX"
  , "19 20 SSE MMX0"
  , "19 19 MEMORY StackMem"
  , "20 29 INTEGER RDX"
  , "20 21 SSE MMX1"
  , "20 20 MEMORY StackMem"
  , "21 30 INTEGER RDX"
  , "21 22 SSE MMX2"
  , "21 21 MEMORY StackMem"
  , "22 31 INTEGER RDX"
  , "22 23 SSE MMX3"
  , "22 22 MEMORY StackMem"
  , "23 32 INTEGER RDX"
  , "23 24 SSE MMX4"
  , "23 23 MEMORY StackMem"
  , "24 33 INTEGER RDX"
  , "24 25 SSE MMX5"
  , "24 24 MEMORY StackMem"
  , "25 34 INTEGER RDX"
  , "25 26 SSE MMX6"
  , "25 25 MEMORY StackMem"
  , "26 35 INTEGER RDX"
  , "26 27 SSE MMX7"
  , "26 26 MEMORY StackMem"
  , "27 36 INTEGER RDX"
  , "27 27 SSE StackFloat"
  , "27 27 MEMORY StackMem"
  , "28 37 INTEGER RCX"
  , "28 29 SSE MMX0"
  , "28 28 MEMORY StackMem"
  , "29 38 INTEGER RCX"
  , "29 30 SSE MMX1"
  , "29 29 MEMORY StackMem"
  , "30 39 INTEGER RCX"
  , "30 31 SSE MMX2"
  , "30 30 MEMORY StackMem"
  , "31 40 INTEGER RCX"
  , "31 32 SSE MMX3"
  , "31 31 MEMORY StackMem"
  , "32 41 INTEGER RCX"
  , "32 33 SSE MMX4"
  , "32 32 MEMORY StackMem"
  , "33 42 INTEGER RCX"
  , "33 34 SSE MMX5"
  , "33 33 MEMORY StackMem"
  , "34 43 INTEGER RCX"
  , "34 35 SSE MMX6"
  , "34 34 MEMORY StackMem"
  , "35 44 INTEGER RCX"
  , "35 36 SSE MMX7"
  , "35 35 MEMORY StackMem"
  , "36 45 INTEGER RCX"
  , "36 36 SSE StackFloat"
  , "36 36 MEMORY StackMem"
  , "37 46 INTEGER R8"
  , "37 38 SSE MMX0"
  , "37 37 MEMORY StackMem"
  , "38 47 INTEGER R8"
  , "38 39 SSE MMX1"
  , "38 38 MEMORY StackMem"
  , "39 48 INTEGER R8"
  , "39 40 SSE MMX2"
  , "39 39 MEMORY StackMem"
  , "40 49 INTEGER R8"
  , "40 41 SSE MMX3"
  , "40 40 MEMORY StackMem"
  , "41 50 INTEGER R8"
  , "41 42 SSE MMX4"
  , "41 41 MEMORY StackMem"
  , "42 51 INTEGER R8"
  , "42 43 SSE MMX5"
  , "42 42 MEMORY StackMem"
  , "43 52 INTEGER R8"
  , "43 44 SSE MMX6"
  , "43 43 MEMORY StackMem"
  , "44 53 INTEGER R8"
  , "44 45 SSE MMX7"
  , "44 44 MEMORY StackMem"
  , "45 54 INTEGER R8"
  , "45 45 SSE StackFloat"
  , "45 45 MEMORY StackMem"
  , "46 55 INTEGER R9"
  , "46 47 SSE MMX0"
  , "46 46 MEMORY StackMem"
  , "47 56 INTEGER R9"
  , "47 48 SSE MMX1"
  , "47 47 MEMORY StackMem"
  , "48 57 INTEGER R9"
  , "48 49 SSE MMX2"
  , "48 48 MEMORY StackMem"
  , "49 58 INTEGER R9"
  , "49 50 SSE MMX3"
  , "49 49 MEMORY StackMem"
  , "50 59 INTEGER R9"
  , "50 51 SSE MMX4"
  , "50 50 MEMORY StackMem"
  , "51 60 INTEGER R9"
  , "51 52 SSE MMX5"
  , "51 51 MEMORY StackMem"
  , "52 61 INTEGER R9"
  , "52 53 SSE MMX6"
  , "52 52 MEMORY StackMem"
  , "53 62 INTEGER R9"
  , "53 54 SSE MMX7"
  , "53 53 MEMORY StackMem"
  , "54 63 INTEGER R9"
  , "54 54 SSE StackFloat"
  , "54 54 MEMORY StackMem"
  , "55 55 INTEGER StackInt"
  , "55 55 MEMORY StackMem"
  , "55 56 SSE MMX0"
  , "56 56 INTEGER StackInt"
  , "56 56 MEMORY StackMem"
  , "56 57 SSE MMX1"
  , "57 57 INTEGER StackInt"
  , "57 57 MEMORY StackMem"
  , "57 58 SSE MMX2"
  , "58 58 INTEGER StackInt"
  , "58 58 MEMORY StackMem"
  , "58 59 SSE MMX3"
  , "59 59 INTEGER StackInt"
  , "59 59 MEMORY StackMem"
  , "59 60 SSE MMX4"
  , "60 60 INTEGER StackInt"
  , "60 60 MEMORY StackMem"
  , "60 61 SSE MMX5"
  , "61 61 INTEGER StackInt"
  , "61 61 MEMORY StackMem"
  , "61 62 SSE MMX6"
  , "62 62 INTEGER StackInt"
  , "62 62 MEMORY StackMem"
  , "62 63 SSE MMX7"
  , "63 63 INTEGER StackInt"
  , "63 63 SSE StackFloat"
  , "63 63 MEMORY StackMem"
  ]
