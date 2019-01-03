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

import           TestParams
import           TestParser
import           TestTypes

lldbScript :: FilePath
lldbScript = "test/test-data/abide-lldb"

main :: IO ()
main = hspec $ do
  it "Test a very simple program with no stack parameters" $ do
    (a, c) <- doTest trivialTest
    a `shouldBe` c  -- Do we need to sort somehow, or wrap for Eq instance?

  it "Test some simple stack parameters" $ do
    (a, c) <- doTest easyStackTest
    a `shouldBe` c

type TestResult = [(CType, Either X86_64Registers StackOffset)]

doTest :: (FilePath, [(CType, Word64)]) -> IO (TestResult, TestResult)
doTest ps = do
  let abideTest = abideParamList (map fst $ snd ps)
  cTest <- uncurry cParamList ps
  return (abideTest, cTest)

--------------------------------------------------------------------------------
-- Utility stuff

-- For a list of parameter types (assumed to be in order), we figure out where
-- Abide thinks each one should be passed.
abideParamList :: [CType] -> [(CType, Either X86_64Registers StackOffset)]
abideParamList ps =
  zip ps $ map (computeParam (Proxy @(X86_64, SystemV))) (tail $ inits ps)

-- For C, we take a file name (should be an executable conforming to a few
-- simple rules, for now just X64) and a list of parameters which have been
-- mapped to a magic, unique, known value.  We then use LLDB to dump the state
-- of the registers when the function is called (those same magic values must
-- be used in the source code) and locate which registers hold which magic
-- values.
cParamList :: FilePath -> [(CType, Word64)] -> IO [(CType, Either X86_64Registers StackOffset)]
cParamList fp params = do
  rvs <- dumpAndParse fp params
  return $ matchWithDump params rvs

-- Get the LLDB dump and call the parser
dumpAndParse :: FilePath -> [(CType, Word64)] -> IO (RegVals, StackVals)
dumpAndParse fp params = return . (`parseDump` params) . T.lines =<< lldbDump fp

-- Call LLDB.  This should probably be made configurable in some fashion.
lldbDump :: FilePath -> IO T.Text
lldbDump fp = do
  (ec, out, err) <- IO.readProcessWithExitCode "lldb" [fp, "-s", lldbScript] ""
  return $ T.pack out

-- Given the known parameters/magic values, match them up with the values
-- extracted from the dump.
matchWithDump
  :: [(CType, Word64)]
  -> (RegVals, StackVals)
  -> [(CType, Either X86_64Registers StackOffset)]
matchWithDump cs vs =
  concatMap matchReg cs ++ concatMap matchStack cs
    where
      rvs = M.toList $ fst vs
      svs = M.toList $ snd vs
      matchReg (ct, w) = case find (\v -> w == snd v) rvs of
        Just (reg, _) -> [(ct, Left reg)]
        Nothing -> []

      matchStack (ct, w) = case find (\v -> w == fst v) svs of
        Just (_, so) -> [(ct, Right so)]
        Nothing -> []
