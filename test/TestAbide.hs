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

main :: IO ()
main = hspec $
  it "simple read" $ do
    c <- cTrivial
    a <- aTrivial
    0 `shouldBe` 0

cTrivial = return $ abideParamList (map fst $ snd trivialParams)

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
  -- print raw
  return M.empty  -- stub

lldbScript :: FilePath
lldbScript = "test/test-data/abide-lldb"

lldbDump :: FilePath -> IO T.Text
lldbDump fp = do
  (ec, out, err) <- IO.readProcessWithExitCode "lldb" [fp, "-s", lldbScript] ""
  return $ T.pack out

