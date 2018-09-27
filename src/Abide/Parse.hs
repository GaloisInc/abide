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
import Abide.Types.Arch.PPC as PPC
import Abide.Types.Arch.X86_64 as X64

x86_64FSTFromFile :: FilePath -> IO (Either (MP.ParseErrorBundle T.Text T.Text) (FST X86_64 SystemV))
x86_64FSTFromFile fp = MP.parse (parseFST x64Classes x64Registers) fp <$> T.readFile fp

--------------------------------------------------------------------------------
-- Below here is a big hack.  This is a temporary until we have a plan for how
-- to make the parsed FSTs available without having to recompute them
-- constantly.

x86_64FST :: FST X86_64 SystemV
x86_64FST = case MP.parse (parseFST x64Classes x64Registers) "" x86_64FSTText of
  Right fst -> fst

x86_64FSTText :: T.Text
x86_64FSTText = T.unlines 
  [ "1 10 INTEGER RDI"
  , "1 2 SSE YMM0"
  , "1 1 MEMORY StackMem"
  , "2 11 INTEGER RDI"
  , "2 3 SSE YMM1"
  , "2 2 MEMORY StackMem"
  , "3 12 INTEGER RDI"
  , "3 4 SSE YMM2"
  , "3 3 MEMORY StackMem"
  , "4 13 INTEGER RDI"
  , "4 5 SSE YMM3"
  , "4 4 MEMORY StackMem"
  , "5 14 INTEGER RDI"
  , "5 6 SSE YMM4"
  , "5 5 MEMORY StackMem"
  , "6 15 INTEGER RDI"
  , "6 7 SSE YMM5"
  , "6 6 MEMORY StackMem"
  , "7 16 INTEGER RDI"
  , "7 8 SSE YMM6"
  , "7 7 MEMORY StackMem"
  , "8 17 INTEGER RDI"
  , "8 9 SSE YMM7"
  , "8 8 MEMORY StackMem"
  , "9 18 INTEGER RDI"
  , "9 9 SSE StackFloat"
  , "9 9 MEMORY StackMem"
  , "10 19 INTEGER RSI"
  , "10 11 SSE YMM0"
  , "10 10 MEMORY StackMem"
  , "11 20 INTEGER RSI"
  , "11 12 SSE YMM1"
  , "11 11 MEMORY StackMem"
  , "12 21 INTEGER RSI"
  , "12 13 SSE YMM2"
  , "12 12 MEMORY StackMem"
  , "13 22 INTEGER RSI"
  , "13 14 SSE YMM3"
  , "13 13 MEMORY StackMem"
  , "14 23 INTEGER RSI"
  , "14 15 SSE YMM4"
  , "14 14 MEMORY StackMem"
  , "15 24 INTEGER RSI"
  , "15 16 SSE YMM5"
  , "15 15 MEMORY StackMem"
  , "16 25 INTEGER RSI"
  , "16 17 SSE YMM6"
  , "16 16 MEMORY StackMem"
  , "17 26 INTEGER RSI"
  , "17 18 SSE YMM7"
  , "17 17 MEMORY StackMem"
  , "18 27 INTEGER RSI"
  , "18 18 SSE StackFloat"
  , "18 18 MEMORY StackMem"
  , "19 28 INTEGER RDX"
  , "19 20 SSE YMM0"
  , "19 19 MEMORY StackMem"
  , "20 29 INTEGER RDX"
  , "20 21 SSE YMM1"
  , "20 20 MEMORY StackMem"
  , "21 30 INTEGER RDX"
  , "21 22 SSE YMM2"
  , "21 21 MEMORY StackMem"
  , "22 31 INTEGER RDX"
  , "22 23 SSE YMM3"
  , "22 22 MEMORY StackMem"
  , "23 32 INTEGER RDX"
  , "23 24 SSE YMM4"
  , "23 23 MEMORY StackMem"
  , "24 33 INTEGER RDX"
  , "24 25 SSE YMM5"
  , "24 24 MEMORY StackMem"
  , "25 34 INTEGER RDX"
  , "25 26 SSE YMM6"
  , "25 25 MEMORY StackMem"
  , "26 35 INTEGER RDX"
  , "26 27 SSE YMM7"
  , "26 26 MEMORY StackMem"
  , "27 36 INTEGER RDX"
  , "27 27 SSE StackFloat"
  , "27 27 MEMORY StackMem"
  , "28 37 INTEGER RCX"
  , "28 29 SSE YMM0"
  , "28 28 MEMORY StackMem"
  , "29 38 INTEGER RCX"
  , "29 30 SSE YMM1"
  , "29 29 MEMORY StackMem"
  , "30 39 INTEGER RCX"
  , "30 31 SSE YMM2"
  , "30 30 MEMORY StackMem"
  , "31 40 INTEGER RCX"
  , "31 32 SSE YMM3"
  , "31 31 MEMORY StackMem"
  , "32 41 INTEGER RCX"
  , "32 33 SSE YMM4"
  , "32 32 MEMORY StackMem"
  , "33 42 INTEGER RCX"
  , "33 34 SSE YMM5"
  , "33 33 MEMORY StackMem"
  , "34 43 INTEGER RCX"
  , "34 35 SSE YMM6"
  , "34 34 MEMORY StackMem"
  , "35 44 INTEGER RCX"
  , "35 36 SSE YMM7"
  , "35 35 MEMORY StackMem"
  , "36 45 INTEGER RCX"
  , "36 36 SSE StackFloat"
  , "36 36 MEMORY StackMem"
  , "37 46 INTEGER R8"
  , "37 38 SSE YMM0"
  , "37 37 MEMORY StackMem"
  , "38 47 INTEGER R8"
  , "38 39 SSE YMM1"
  , "38 38 MEMORY StackMem"
  , "39 48 INTEGER R8"
  , "39 40 SSE YMM2"
  , "39 39 MEMORY StackMem"
  , "40 49 INTEGER R8"
  , "40 41 SSE YMM3"
  , "40 40 MEMORY StackMem"
  , "41 50 INTEGER R8"
  , "41 42 SSE YMM4"
  , "41 41 MEMORY StackMem"
  , "42 51 INTEGER R8"
  , "42 43 SSE YMM5"
  , "42 42 MEMORY StackMem"
  , "43 52 INTEGER R8"
  , "43 44 SSE YMM6"
  , "43 43 MEMORY StackMem"
  , "44 53 INTEGER R8"
  , "44 45 SSE YMM7"
  , "44 44 MEMORY StackMem"
  , "45 54 INTEGER R8"
  , "45 45 SSE StackFloat"
  , "45 45 MEMORY StackMem"
  , "46 55 INTEGER R9"
  , "46 47 SSE YMM0"
  , "46 46 MEMORY StackMem"
  , "47 56 INTEGER R9"
  , "47 48 SSE YMM1"
  , "47 47 MEMORY StackMem"
  , "48 57 INTEGER R9"
  , "48 49 SSE YMM2"
  , "48 48 MEMORY StackMem"
  , "49 58 INTEGER R9"
  , "49 50 SSE YMM3"
  , "49 49 MEMORY StackMem"
  , "50 59 INTEGER R9"
  , "50 51 SSE YMM4"
  , "50 50 MEMORY StackMem"
  , "51 60 INTEGER R9"
  , "51 52 SSE YMM5"
  , "51 51 MEMORY StackMem"
  , "52 61 INTEGER R9"
  , "52 53 SSE YMM6"
  , "52 52 MEMORY StackMem"
  , "53 62 INTEGER R9"
  , "53 54 SSE YMM7"
  , "53 53 MEMORY StackMem"
  , "54 63 INTEGER R9"
  , "54 54 SSE StackFloat"
  , "54 54 MEMORY StackMem"
  , "55 55 INTEGER StackInt"
  , "55 55 MEMORY StackMem"
  , "55 56 SSE YMM0"
  , "56 56 INTEGER StackInt"
  , "56 56 MEMORY StackMem"
  , "56 57 SSE YMM1"
  , "57 57 INTEGER StackInt"
  , "57 57 MEMORY StackMem"
  , "57 58 SSE YMM2"
  , "58 58 INTEGER StackInt"
  , "58 58 MEMORY StackMem"
  , "58 59 SSE YMM3"
  , "59 59 INTEGER StackInt"
  , "59 59 MEMORY StackMem"
  , "59 60 SSE YMM4"
  , "60 60 INTEGER StackInt"
  , "60 60 MEMORY StackMem"
  , "60 61 SSE YMM5"
  , "61 61 INTEGER StackInt"
  , "61 61 MEMORY StackMem"
  , "61 62 SSE YMM6"
  , "62 62 INTEGER StackInt"
  , "62 62 MEMORY StackMem"
  , "62 63 SSE YMM7"
  , "63 63 INTEGER StackInt"
  , "63 63 SSE StackFloat"
  , "63 63 MEMORY StackMem"
  ]

ppcFSTFromFile :: FilePath -> IO (Either (MP.ParseErrorBundle T.Text T.Text) (FST PPC SystemV))
ppcFSTFromFile fp = MP.parse (parseFST ppcClasses ppcRegisters) fp <$> T.readFile fp

ppcFST :: FST PPC SystemV
ppcFST = case MP.parse (parseFST ppcClasses ppcRegisters) "" ppcFSTText of
  Right fst -> fst

ppcFSTText :: T.Text
ppcFSTText = T.unlines
  [ "1 15 PPCGP R3"
  , "1 2 PPCFLOAT F1"
  , "2 16 PPCGP R3"
  , "2 3 PPCFLOAT F2"
  , "3 17 PPCGP R3"
  , "3 4 PPCFLOAT F3"
  , "4 18 PPCGP R3"
  , "4 5 PPCFLOAT F4"
  , "5 19 PPCGP R3"
  , "5 6 PPCFLOAT F5"
  , "6 20 PPCGP R3"
  , "6 7 PPCFLOAT F6"
  , "7 21 PPCGP R3"
  , "7 8 PPCFLOAT F7"
  , "8 22 PPCGP R3"
  , "8 9 PPCFLOAT F8"
  , "9 23 PPCGP R3"
  , "9 10 PPCFLOAT F9"
  , "10 24 PPCGP R3"
  , "10 11 PPCFLOAT F10"
  , "11 25 PPCGP R3"
  , "11 12 PPCFLOAT F11"
  , "12 26 PPCGP R3"
  , "12 13 PPCFLOAT F12"
  , "13 27 PPCGP R3"
  , "13 14 PPCFLOAT F13"
  , "14 28 PPCGP R3"
  , "14 14 PPCFLOAT StackFloat"
  , "15 29 PPCGP R4"
  , "15 16 PPCFLOAT F1"
  , "16 30 PPCGP R4"
  , "16 17 PPCFLOAT F2"
  , "17 31 PPCGP R4"
  , "17 18 PPCFLOAT F3"
  , "18 32 PPCGP R4"
  , "18 19 PPCFLOAT F4"
  , "19 33 PPCGP R4"
  , "19 20 PPCFLOAT F5"
  , "20 34 PPCGP R4"
  , "20 21 PPCFLOAT F6"
  , "21 35 PPCGP R4"
  , "21 22 PPCFLOAT F7"
  , "22 36 PPCGP R4"
  , "22 23 PPCFLOAT F8"
  , "23 37 PPCGP R4"
  , "23 24 PPCFLOAT F9"
  , "24 38 PPCGP R4"
  , "24 25 PPCFLOAT F10"
  , "25 39 PPCGP R4"
  , "25 26 PPCFLOAT F11"
  , "26 40 PPCGP R4"
  , "26 27 PPCFLOAT F12"
  , "27 41 PPCGP R4"
  , "27 28 PPCFLOAT F13"
  , "28 42 PPCGP R4"
  , "28 28 PPCFLOAT StackFloat"
  , "29 43 PPCGP R5"
  , "29 30 PPCFLOAT F1"
  , "30 44 PPCGP R5"
  , "30 31 PPCFLOAT F2"
  , "31 45 PPCGP R5"
  , "31 32 PPCFLOAT F3"
  , "32 46 PPCGP R5"
  , "32 33 PPCFLOAT F4"
  , "33 47 PPCGP R5"
  , "33 34 PPCFLOAT F5"
  , "34 48 PPCGP R5"
  , "34 35 PPCFLOAT F6"
  , "35 49 PPCGP R5"
  , "35 36 PPCFLOAT F7"
  , "36 50 PPCGP R5"
  , "36 37 PPCFLOAT F8"
  , "37 51 PPCGP R5"
  , "37 38 PPCFLOAT F9"
  , "38 52 PPCGP R5"
  , "38 39 PPCFLOAT F10"
  , "39 53 PPCGP R5"
  , "39 40 PPCFLOAT F11"
  , "40 54 PPCGP R5"
  , "40 41 PPCFLOAT F12"
  , "41 55 PPCGP R5"
  , "41 42 PPCFLOAT F13"
  , "42 56 PPCGP R5"
  , "42 42 PPCFLOAT StackFloat"
  , "43 57 PPCGP R6"
  , "43 44 PPCFLOAT F1"
  , "44 58 PPCGP R6"
  , "44 45 PPCFLOAT F2"
  , "45 59 PPCGP R6"
  , "45 46 PPCFLOAT F3"
  , "46 60 PPCGP R6"
  , "46 47 PPCFLOAT F4"
  , "47 61 PPCGP R6"
  , "47 48 PPCFLOAT F5"
  , "48 62 PPCGP R6"
  , "48 49 PPCFLOAT F6"
  , "49 63 PPCGP R6"
  , "49 50 PPCFLOAT F7"
  , "50 64 PPCGP R6"
  , "50 51 PPCFLOAT F8"
  , "51 65 PPCGP R6"
  , "51 52 PPCFLOAT F9"
  , "52 66 PPCGP R6"
  , "52 53 PPCFLOAT F10"
  , "53 67 PPCGP R6"
  , "53 54 PPCFLOAT F11"
  , "54 68 PPCGP R6"
  , "54 55 PPCFLOAT F12"
  , "55 69 PPCGP R6"
  , "55 56 PPCFLOAT F13"
  , "56 70 PPCGP R6"
  , "56 56 PPCFLOAT StackFloat"
  , "57 71 PPCGP R7"
  , "57 58 PPCFLOAT F1"
  , "58 72 PPCGP R7"
  , "58 59 PPCFLOAT F2"
  , "59 73 PPCGP R7"
  , "59 60 PPCFLOAT F3"
  , "60 74 PPCGP R7"
  , "60 61 PPCFLOAT F4"
  , "61 75 PPCGP R7"
  , "61 62 PPCFLOAT F5"
  , "62 76 PPCGP R7"
  , "62 63 PPCFLOAT F6"
  , "63 77 PPCGP R7"
  , "63 64 PPCFLOAT F7"
  , "64 78 PPCGP R7"
  , "64 65 PPCFLOAT F8"
  , "65 79 PPCGP R7"
  , "65 66 PPCFLOAT F9"
  , "66 80 PPCGP R7"
  , "66 67 PPCFLOAT F10"
  , "67 81 PPCGP R7"
  , "67 68 PPCFLOAT F11"
  , "68 82 PPCGP R7"
  , "68 69 PPCFLOAT F12"
  , "69 83 PPCGP R7"
  , "69 70 PPCFLOAT F13"
  , "70 84 PPCGP R7"
  , "70 70 PPCFLOAT StackFloat"
  , "71 85 PPCGP R8"
  , "71 72 PPCFLOAT F1"
  , "72 86 PPCGP R8"
  , "72 73 PPCFLOAT F2"
  , "73 87 PPCGP R8"
  , "73 74 PPCFLOAT F3"
  , "74 88 PPCGP R8"
  , "74 75 PPCFLOAT F4"
  , "75 89 PPCGP R8"
  , "75 76 PPCFLOAT F5"
  , "76 90 PPCGP R8"
  , "76 77 PPCFLOAT F6"
  , "77 91 PPCGP R8"
  , "77 78 PPCFLOAT F7"
  , "78 92 PPCGP R8"
  , "78 79 PPCFLOAT F8"
  , "79 93 PPCGP R8"
  , "79 80 PPCFLOAT F9"
  , "80 94 PPCGP R8"
  , "80 81 PPCFLOAT F10"
  , "81 95 PPCGP R8"
  , "81 82 PPCFLOAT F11"
  , "82 96 PPCGP R8"
  , "82 83 PPCFLOAT F12"
  , "83 97 PPCGP R8"
  , "83 84 PPCFLOAT F13"
  , "84 98 PPCGP R8"
  , "84 84 PPCFLOAT StackFloat"
  , "85 99 PPCGP R9"
  , "85 86 PPCFLOAT F1"
  , "86 100 PPCGP R9"
  , "86 87 PPCFLOAT F2"
  , "87 101 PPCGP R9"
  , "87 88 PPCFLOAT F3"
  , "88 102 PPCGP R9"
  , "88 89 PPCFLOAT F4"
  , "89 103 PPCGP R9"
  , "89 90 PPCFLOAT F5"
  , "90 104 PPCGP R9"
  , "90 91 PPCFLOAT F6"
  , "91 105 PPCGP R9"
  , "91 92 PPCFLOAT F7"
  , "92 106 PPCGP R9"
  , "92 93 PPCFLOAT F8"
  , "93 107 PPCGP R9"
  , "93 94 PPCFLOAT F9"
  , "94 108 PPCGP R9"
  , "94 95 PPCFLOAT F10"
  , "95 109 PPCGP R9"
  , "95 96 PPCFLOAT F11"
  , "96 110 PPCGP R9"
  , "96 97 PPCFLOAT F12"
  , "97 111 PPCGP R9"
  , "97 98 PPCFLOAT F13"
  , "98 112 PPCGP R9"
  , "98 98 PPCFLOAT StackFloat"
  , "99 113 PPCGP R10"
  , "99 100 PPCFLOAT F1"
  , "100 114 PPCGP R10"
  , "100 101 PPCFLOAT F2"
  , "101 115 PPCGP R10"
  , "101 102 PPCFLOAT F3"
  , "102 116 PPCGP R10"
  , "102 103 PPCFLOAT F4"
  , "103 117 PPCGP R10"
  , "103 104 PPCFLOAT F5"
  , "104 118 PPCGP R10"
  , "104 105 PPCFLOAT F6"
  , "105 119 PPCGP R10"
  , "105 106 PPCFLOAT F7"
  , "106 120 PPCGP R10"
  , "106 107 PPCFLOAT F8"
  , "107 121 PPCGP R10"
  , "107 108 PPCFLOAT F9"
  , "108 122 PPCGP R10"
  , "108 109 PPCFLOAT F10"
  , "109 123 PPCGP R10"
  , "109 110 PPCFLOAT F11"
  , "110 124 PPCGP R10"
  , "110 111 PPCFLOAT F12"
  , "111 125 PPCGP R10"
  , "111 112 PPCFLOAT F13"
  , "112 126 PPCGP R10"
  , "112 112 PPCFLOAT StackFloat"
  , "113 113 PPCGP StackGP"
  , "113 114 PPCFLOAT F1"
  , "114 114 PPCGP StackGP"
  , "114 115 PPCFLOAT F2"
  , "115 115 PPCGP StackGP"
  , "115 116 PPCFLOAT F3"
  , "116 116 PPCGP StackGP"
  , "116 117 PPCFLOAT F4"
  , "117 117 PPCGP StackGP"
  , "117 118 PPCFLOAT F5"
  , "118 118 PPCGP StackGP"
  , "118 119 PPCFLOAT F6"
  , "119 119 PPCGP StackGP"
  , "119 120 PPCFLOAT F7"
  , "120 120 PPCGP StackGP"
  , "120 121 PPCFLOAT F8"
  , "121 121 PPCGP StackGP"
  , "121 122 PPCFLOAT F9"
  , "122 122 PPCGP StackGP"
  , "122 123 PPCFLOAT F10"
  , "123 123 PPCGP StackGP"
  , "123 124 PPCFLOAT F11"
  , "124 124 PPCGP StackGP"
  , "124 125 PPCFLOAT F12"
  , "125 125 PPCGP StackGP"
  , "125 126 PPCFLOAT F13"
  , "126 126 PPCGP StackGP"
  , "126 126 PPCFLOAT StackFloat"
  ]
