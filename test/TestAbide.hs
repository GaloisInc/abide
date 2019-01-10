{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.List ( find, inits )
import qualified Data.Map.Strict as M
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

-- | The location of the LLDB test script.
lldbScript :: FilePath
lldbScript = "test/test-data/abide-lldb"

main :: IO ()
main = hspec $ do
  it "Test parameters that all fit in registers" $ do
    (a, c) <- doTest regTest
    True `shouldBe` True  -- Do we need to sort somehow, or wrap for Eq instance?

  -- it "Test integer parameters passed on the stack" $ do
  --   (a, c) <- doTest intStackTest
  --   a `shouldBe` c

  -- it "Test float parameters passed on the stack" $ do
  --   (a, c) <- doTest floatStackTest
  --   a `shouldBe` c

type TestResult = [(CType, Either X86_64Registers StackOffset)]

-- | Run a given test through abide and the LLDB dump parser, and return both
-- results.
doTest :: (FilePath, [(CType, Word64)]) -> IO (TestResult, TestResult)
doTest ps = do
  let abideTest = abideParamList (map fst $ snd ps)
  cTest <- cParamList (snd ps)
  return (abideTest, cTest)

--------------------------------------------------------------------------------
-- Utility stuff

-- | For a list of parameter types (assumed to be in order), we figure out
-- where Abide thinks each one should be passed.
abideParamList :: [CType] -> [(CType, Either X86_64Registers StackOffset)]
abideParamList ps =
  zip ps $ map (computeParam (Proxy @(X86_64, SystemV))) (tail $ inits ps)

-- | For a kind of pseudo-oracle result, we generate a C file with some inline
-- assembly to dump register contents and examine the stack, and then parse
-- those results.
cParamList :: FnParamSpec -> IO [(CType, Either X86_64Registers StackOffset)]
cParamList params = do
  rvs <- dumpAndParse params
  return $ matchWithDump params rvs

-- | For a specification, get the dump from the C generating module and then
-- parse the output.
dumpAndParse :: FnParamSpec -> IO (RegVals, StackVals)
dumpAndParse params = doCTest params >>= return . parseCout params

-- | Given the known parameters/magic values, match them up with the values
-- extracted from the dump.
matchWithDump
  :: FnParamSpec
  -> (RegVals, StackVals)
  -> [(CType, Either X86_64Registers StackOffset)]
matchWithDump cs vs = concatMap matchReg cs ++ concatMap matchStack cs
    where
      rvs = M.toList $ fst vs
      svs = M.toList $ snd vs
      matchReg (ct, w) = case find (\v -> w == snd v) rvs of
        Just (reg, _) -> [(ct, Left reg)]
        Nothing -> []
      matchStack (ct, w) = case find (\v -> w == fst v) svs of
        Just (_, so) -> [(ct, Right so)]
        Nothing -> []
