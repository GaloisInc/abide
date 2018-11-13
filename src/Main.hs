{-# LANGUAGE DataKinds #-}

module Main where

import System.FilePath ( (</>) )

import Abide.ABI.SystemV as SV
import Abide.Generate
import Abide.Parse

main :: IO ()
main = generatePPC32 >> generatePPC64 >> generateX64

generateX64 :: IO ()
generateX64 =
  generateFiles "fst-files/x86_64" (mkFSTGen SV.x86_64Typemap)

generatePPC32 :: IO ()
generatePPC32 =
  generateFiles "fst-files/ppc32" (mkFSTGen SV.ppc32Typemap)

generatePPC64 :: IO ()
generatePPC64 =
  generateFiles "fst-files/ppc64" (mkFSTGen SV.ppc64Typemap)
