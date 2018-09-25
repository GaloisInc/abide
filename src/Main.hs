{-# LANGUAGE DataKinds #-}

module Main where

import System.FilePath ( (</>) )

import Abide.ABI.SystemV as SV
import Abide.Compute
import Abide.Generate
import Abide.Parse
import Abide.Types
import Abide.Types.ABI.SystemV
import Abide.Types.Arch.X86_64

main :: IO ()
main = generateX86

-- An example of generating the X86_64 finite state transducer.  We need a
-- better story for generating these and then parsing them for use without
-- having to repeat the work.
generateX86 :: IO ()
generateX86 = do
  generateFiles ("/home/karl/code/abide/test/" </> SV.x86fp) (mkFSTGen SV.x86_64Typemap)
  efst <- x86_64FSTFromFile "/home/karl/code/abide/test/x86_64.fst.txt"
  case efst of
    Left err -> print err
    Right fst -> print $ transduce fst fakeInput

fakeInput = [INTEGER, INTEGER, SSE]
