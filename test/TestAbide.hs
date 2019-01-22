{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.List ( find, inits, sortOn )
import qualified Data.Map.Strict as M
import           Data.Maybe ( catMaybes )
import           Data.Ord ( comparing )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Word
import qualified Language.C.Quote as C
import qualified System.Process as IO
import           Test.Hspec

import           Abide.Compute
import           Abide.CTypes
import           Abide.Types
import qualified Abide.Parse.Arch as AP
import qualified Abide.Types.Arch.PPC32 as P32
import qualified Abide.Types.Arch.PPC64 as P64
import           Abide.Types.Arch.X86_64


import           TestGenerator
import           TestParams
import           TestParser
import           TestTypes

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Error as MP
import qualified Text.PrettyPrint.Mainland as PP
import qualified Text.PrettyPrint.Mainland.Class as PP
karl :: IO ()
karl = PP.pprint (mkCGenerator (Proxy @(PPC32, SystemV)) intStackTest)

karltxt = T.lines "R3 : 11\nR4 : 22\nR5 : 33\nR6 : 44\nR7 : 55\nR8 : 66\nR9 : 0\nR10 : 0\nF1 : 0\nF2 : 0"

karlp :: IO ()
karlp = print $ parseRegs (Proxy @(PPC32, SystemV)) karltxt

-- karlp :: IO ()
-- karlp = case MP.parse (parseRegs (Proxy @(PPC32, SystemV))) "" karltxt of
--   Right a -> print a
--   Left e -> print e

px64 = Proxy @(X86_64, SystemV)
pp32 = Proxy @(PPC32, SystemV)

main :: IO ()
main = hspec $ do
  let px64 = Proxy @(X86_64, SystemV)
      pp32 = Proxy @(PPC32, SystemV)

  -- it "Test parameters that all fit in registers" $ do
  --   (aRes, cRes) <- doTest pp32 regTest
  --   aRes `shouldBe` cRes

  it "Test integer parameters passed on the stack" $ do
    (aRes, cRes) <- doTest pp32 intStackTest
    aRes `shouldBe` cRes

  it "Test float parameters passed on the stack" $ do
    (aRes, cRes) <- doTest pp32 floatStackTest
    aRes `shouldBe` cRes

--------------------------------------------------------------------------------
-- Preliminary types and data needed for testing

type TestResult reg = [(CType, Either reg StackOffset)]

instance TestableArch X86_64 SystemV where
  regParser = AP.x64Registers
  regStrings _ = x64RegStrs
  regVarNames _ = x64RegVariables
  gccFP _ = "gcc"
  mkRegAsmFloat p = mkX64RegAsmFloat
  mkRegAsmInt p = mkX64RegAsmInt
  mkStackAsm p = mkX64StackAsm
  exeWrapper _ x = (x, [])

instance TestableArch PPC32 SystemV where
  regParser = AP.ppc32Registers
  regStrings _ = ppcRegStrs
  regVarNames _ = ppcRegVariables
  gccFP _ = "powerpc-linux-gnu-gcc"
  mkRegAsmFloat _ = mkPPCRegAsmFloat
  mkRegAsmInt _ = mkPPCRegAsmInt
  mkStackAsm _ = mkPPCStackAsm
  exeWrapper _ x = ("qemu-ppc", [x])


x64RegStrs :: [T.Text]
x64RegStrs = [ "RDI", "RSI", "RDX", "RCX", "R8", "R9"
             , "XMM0", "XMM1", "XMM2", "XMM3", "XMM4", "XMM5", "XMM6", "XMM7" ]

ppcRegStrs :: [T.Text]
ppcRegStrs = [ "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10"
             , "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12", "F13" ]

x64RegVariables :: [(X86_64Registers, T.Text)]
x64RegVariables =
  let regs = [ RDI , RSI , RDX , RCX , R8 , R9
             , XMM0 , XMM1 , XMM2 , XMM3 , XMM4 , XMM5 , XMM6 , XMM7 ]
  in zip regs x64RegStrs

ppcRegVariables :: [(P32.PPC32Registers, T.Text)]
ppcRegVariables =
  let regs = [ P32.R3, P32.R4, P32.R5, P32.R6, P32.R7, P32.R8, P32.R9
             , P32.F1, P32.F2, P32.F3, P32.F4, P32.F5, P32.F6, P32.F7, P32.F8, P32.F9, P32.F10, P32.F11, P32.F12, P32.F13 ]
  in zip regs ppcRegStrs


--------------------------------------------------------------------------------
-- Actually run tests

-- | Run a given test through abide and the LLDB dump parser, and return both
-- results.
doTest
  :: ( ArchParamBaseOffset arch
     , CTypeInput arch abi
     , Eq (InSymbol arch abi)
     , IsStack reg
     , IsFPReg reg
     , OutSymbol arch abi ~ reg
     , ParamABI arch abi
     , TestableArch arch abi
     )
  => proxy (arch, abi) -> [(CType, Word64)] -> IO (TestResult reg, TestResult reg)
doTest px ps = (abideParamList px (map fst ps), ) <$> cParamList px ps

--------------------------------------------------------------------------------
-- Utility stuff

-- | For a list of parameter types (assumed to be in order), we figure out
-- where Abide thinks each one should be passed.
abideParamList
  :: ( ArchParamBaseOffset arch
     , CTypeInput arch abi
     , Eq (InSymbol arch abi)
     , IsStack reg
     , OutSymbol arch abi ~ reg
     , ParamABI arch abi
     , TestableArch arch abi
     )
  => proxy (arch, abi) -> [CType] -> [(CType, Either reg StackOffset)]
abideParamList px ps =
  zip ps $ map (computeParam px) (tail $ inits ps)

-- | For a kind of pseudo-oracle result, we generate a C file with some inline
-- assembly to dump register contents and examine the stack, and then parse
-- those results.
cParamList
  :: ( OutSymbol arch abi ~ reg
     , TestableArch arch abi
     , IsFPReg reg
     )
  => proxy (arch, abi) -> FnParamSpec -> IO [(CType, Either reg StackOffset)]
cParamList px params = do
  (rvs,svs) <- dumpAndParse px params
  return $ matchWithParse params (sortOn fst (M.toList rvs)) (sortOn fst (M.toList svs))

-- | For a specification, get the dump from the C generating module and then
-- parse the output.
dumpAndParse
  :: ( OutSymbol arch abi ~ reg
     , TestableArch arch abi
     , IsFPReg reg
     )
  => proxy (arch, abi) -> FnParamSpec -> IO (RegVals reg, StackVals)
dumpAndParse px params = parseCout px params <$> doCTest px params

-- | Given the known parameters/magic values, match them up with the values
-- extracted from the dump.  Crucially, we need to make sure that parameters
-- we find in registers aren't also found on the stack, since they sometimes
-- show up there.
matchWithParse
  :: FnParamSpec
  -> [(Word64, reg)]
  -> [(Word64, StackOffset)]
  -> [(CType, Either reg StackOffset)]
matchWithParse ps rws wos =
  catMaybes $ map (findAndEither Left) rws ++ map (findAndEither Right) filtWos
    where
      filtWos = filter (notInRegs rws) wos

      notInRegs :: [(Word64, a)] -> (Word64, b) -> Bool
      notInRegs xs x = fst x `notElem` map fst xs

      findParam :: Word64 -> FnParamSpec -> Maybe CType
      findParam w cws = fst <$> find ((== w) . snd) cws

      findAndEither :: (a -> Either b c) -> (Word64, a) -> Maybe (CType, Either b c)
      findAndEither f (w, x) = case findParam w ps of
        Just ct -> Just (ct, f x)
        _ -> Nothing

