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

main :: IO ()
main = hspec $ do
  let x64 = Proxy @(X86_64, SystemV)
      ppc64 = Proxy @(PPC64, SystemV)

  it "Test parameters that all fit in registers ppc64" $ do
    (aRes, cRes) <- doTest ppc64 regTest
    aRes `shouldBe` cRes

  it "Test parameters that all fit in registers x64" $ do
    (aRes, cRes) <- doTest x64 regTest
    aRes `shouldBe` cRes

  it "Test integer parameters passed on the stack ppc64" $ do
    (aRes, cRes) <- doTest ppc64 intStackTest
    aRes `shouldBe` cRes

  it "Test integer parameters passed on the stack x64" $ do
    (aRes, cRes) <- doTest x64 intStackTest
    aRes `shouldBe` cRes

  it "Test float parameters passed on the stack ppc64" $ do
    (aRes, cRes) <- doTest ppc64 floatStackTest
    aRes `shouldBe` cRes

  it "Test float parameters passed on the stack x64" $ do
    (aRes, cRes) <- doTest x64 floatStackTest
    aRes `shouldBe` cRes

--------------------------------------------------------------------------------
-- Preliminary types and data needed for testing

type TestResult reg = [(CType, Either reg StackOffset)]

instance TestableArch X86_64 SystemV where
  regSize _ = CInt64
  numRegs _ = length x64RegStrs
  regParser = AP.x64Registers
  regStrings _ = x64RegStrs
  regVarNames _ = x64RegVariables
  mkAsmHeader _ = mkX64AsmHeader
  mkAsmFooter _ = ["\tret"]
  mkRegHeader _ = []
  mkRegAsm _ = mkX64RegAsm
  mkMemHeader _ = []
  mkMemAsm _ = mkX64MemAsm
  ccFP _ = "gcc"
  ccFlags _ = []
  exeWrapper _ exe = (exe, [])

instance TestableArch PPC64 SystemV where
  regSize _ = CInt64
  numRegs _ = length ppcRegStrs
  regParser = AP.ppc64Registers
  regStrings _ = ppcRegStrs
  regVarNames _ = ppc64RegVariables
  mkAsmHeader _ = mkPPC64AsmHeader
  mkAsmFooter _ = ["\tblr"]
  mkRegHeader _ = mkPPC64RegHeader
  mkRegAsm _ = mkPPC64RegAsm
  mkMemHeader _ = mkPPC64MemHeader
  mkMemAsm _ = mkPPC64MemAsm
  ccFP _ = "powerpc64-linux-gnu-gcc"
  ccFlags _ = []
  exeWrapper _ exe = ("qemu-ppc64", [exe])

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

ppc64RegVariables :: [(P64.PPC64Registers, T.Text)]
ppc64RegVariables =
  let regs = [ P64.R3, P64.R4, P64.R5, P64.R6, P64.R7, P64.R8, P64.R9, P64.R10
             , P64.F1, P64.F2, P64.F3, P64.F4, P64.F5, P64.F6, P64.F7, P64.F8, P64.F9, P64.F10, P64.F11, P64.F12, P64.F13 ]
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

