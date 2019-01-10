{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.List ( find, inits, sortBy )
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
  it "Test parameters that all fit in registers" $ do
    (a, c) <- doTest intStackTest
    a `shouldBe` c  -- Do we need to sort somehow, or wrap for Eq instance?

  -- it "Test integer parameters passed on the stack" $ do
  --   (a, c) <- doTest intStackTest
  --   a `shouldBe` c

  -- it "Test float parameters passed on the stack" $ do
  --   (a, c) <- doTest floatStackTest
  --   a `shouldBe` c

type TestResult = [(CType, Either X86_64Registers StackOffset)]

-- | Run a given test through abide and the LLDB dump parser, and return both
-- results.
doTest :: [(CType, Word64)] -> IO (TestResult, TestResult)
doTest ps = do
  let abideTest = abideParamList (map fst ps)
  cTest <- cParamList ps
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
  (rvs',svs') <- dumpAndParse params
  let rvs = sortBy (comparing snd) (M.toList rvs')
  let svs = sortBy (comparing fst) (M.toList svs')
  return $ matchWithParse params rvs svs

-- | For a specification, get the dump from the C generating module and then
-- parse the output.
dumpAndParse :: FnParamSpec -> IO (RegVals, StackVals)
dumpAndParse params = parseCout params <$> doCTest params

-- | Given the known parameters/magic values, match them up with the values
-- extracted from the dump.  Crucially, we need to make sure that parameters
-- we find in registers aren't also found on the stack, since they sometimes
-- show up there.
matchWithParse
  :: FnParamSpec
  -> [(X86_64Registers, Word64)]
  -> [(Word64, StackOffset)]
  -> [(CType, Either X86_64Registers StackOffset)]
matchWithParse ps rws wos =
  let filtWos = filter (notInRegs rws) wos
  in catMaybes $ map handleReg rws ++ map handleStack filtWos
    where
      findParam :: Word64 -> FnParamSpec -> Maybe CType
      findParam w cws = fst <$> find ((== w) . snd) cws

      handleReg :: (X86_64Registers, Word64) -> Maybe (CType, Either X86_64Registers StackOffset)
      handleReg (reg, w) = case findParam w ps of
        Just ct -> Just (ct, Left reg)
        Nothing -> Nothing

      handleStack :: (Word64, StackOffset) -> Maybe (CType, Either X86_64Registers StackOffset)
      handleStack (w, so) = case findParam w ps of
        Just ct -> Just (ct, Right so)
        Nothing -> Nothing

      notInRegs :: [(a, Word64)] -> (Word64, b) -> Bool
      notInRegs xs x = not $ fst x `elem` map snd xs
