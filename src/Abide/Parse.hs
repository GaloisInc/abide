{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module : Abide.Parse
Copyright : (c) 2018 Galois Inc.
License : BSD3 (see LICENSE)

This module defines a parser for the FST files.  It's important to generate
the files and parse them rather than implementing the computation directly
because it allows for external verification of the parameter passing rules.
-}

module Abide.Parse where

import qualified Data.ByteString as BS
import qualified Data.FileEmbed as FE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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

instance HasFST X86_64 SystemV where
  getFST = x86_64FST

instance HasFST PPC SystemV where
  getFST = ppcFST

--------------------------------------------------------------------------------
-- Below here is a big hack.  This is a temporary until we have a plan for how
-- to make the parsed FSTs available without having to recompute them
-- constantly.

x86_64FST :: FST X86_64 SystemV
x86_64FST =
  case MP.parse (parseFST x64Classes x64Registers) "" (T.decodeUtf8 x86_64FSTFile) of
    Right fst -> fst

x86_64FSTFile :: BS.ByteString
x86_64FSTFile = $(FE.embedFile "fst-files/x86_64.fst.txt")

ppcFST :: FST PPC SystemV
ppcFST =
  case MP.parse (parseFST ppcClasses ppcRegisters) "" (T.decodeUtf8 ppcFSTFile) of
    Right fst -> fst

ppcFSTFile :: BS.ByteString
ppcFSTFile = $(FE.embedFile "fst-files/ppc.fst.txt")
