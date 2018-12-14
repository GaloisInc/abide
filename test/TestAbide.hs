{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.List ( inits )
import qualified Data.Map as M
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
import           TestTypes

lldbScript :: FilePath
lldbScript = "test/test-data/abide-lldb"

main :: IO ()
main = hspec $
  it "simple read" $ do
    -- c <- cTrivial
    -- a <- aTrivial
    0 `shouldBe` 0  -- stub, tests aren't ready

cTrivial :: IO [(CType, Either X86_64Registers StackOffset)]
cTrivial = return $ abideParamList (map fst $ snd trivialParams)

aTrivial :: IO [(CType, Either X86_64Registers StackOffset)]
aTrivial = (uncurry cParamList) trivialParams

abideParamList :: [CType] -> [(CType, Either X86_64Registers StackOffset)]
abideParamList ps =
  zip ps $ map (computeParam (Proxy @(X86_64, SystemV))) (tail $ inits ps)

cParamList :: FilePath -> [(CType, Word64)] -> IO [(CType, Either X86_64Registers StackOffset)]
cParamList fp params = do
  dump <- dumpDebugInfo fp
  return []  -- stub

dumpDebugInfo :: FilePath -> IO RegVals
dumpDebugInfo fp = do
  raw <- lldbDump fp
  parseRegs (map T.words $ T.lines raw)

parseRegs :: [[T.Text]] -> IO RegVals
parseRegs tts = do
  let relevantLines = filter (\xs -> not (null xs) && (isReg (head xs))) tts
  print relevantLines
  print (length relevantLines)
  return M.empty

isReg :: T.Text -> Bool
isReg txt = elem txt regStrs
  where
    regStrs = [ "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp", "r8", "r9"
              , "ymm0", "ymm1", "ymm2", "ymm3", "ymm4", "ymm5", "ymm6", "ymm7" ]

lldbDump :: FilePath -> IO T.Text
lldbDump fp = do
  (ec, out, err) <- IO.readProcessWithExitCode "lldb" [fp, "-s", lldbScript] ""
  return $ T.pack out

