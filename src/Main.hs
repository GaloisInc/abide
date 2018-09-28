{-# LANGUAGE DataKinds #-}

module Main where

import System.FilePath ( (</>) )

import Abide.ABI.SystemV as SV
import Abide.Generate
import Abide.Parse

main :: IO ()
main = generatePPC >> generateX64

generateX64 :: IO ()
generateX64 =
  generateFiles "fst-files/x86_64" (mkFSTGen SV.x86_64Typemap)

generatePPC :: IO ()
generatePPC =
  generateFiles "fst-files/ppc" (mkFSTGen SV.ppcTypemap)
