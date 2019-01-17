{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
import qualified System.Process as IO
import           Test.Hspec

import           Abide.Compute
import           Abide.CTypes
import           Abide.Types
import           Abide.Types.Arch.X86_64

import           TestGenerator
import           TestParams
import           TestParser
import           TestTypes

main :: IO ()
main = hspec $ do
  let px64 = Proxy @(X86_64, SystemV)

  it "Test parameters that all fit in registers" $ do
    (aRes, cRes) <- doTest px64 regTest
    aRes `shouldBe` cRes

  it "Test integer parameters passed on the stack" $ do
    (aRes, cRes) <- doTest px64 intStackTest
    aRes `shouldBe` cRes

  it "Test float parameters passed on the stack" $ do
    (aRes, cRes) <- doTest px64 floatStackTest
    aRes `shouldBe` cRes

type TestResult reg = [(CType, Either reg StackOffset)]

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

