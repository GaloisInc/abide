{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TestGenerator where

import           Data.Char ( isAlpha )
import           Data.List ( intersperse )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Word
import           Language.C.Pretty ()
import qualified Language.C.Quote as C
import qualified Language.C.Quote.C as C
import           Numeric.Natural
import qualified System.Exit as SE
import           System.FilePath ( (</>) )
import qualified System.FilePath as FP
import qualified System.IO as IO
import qualified System.IO.Temp as Tmp
import qualified System.Process as Proc
import qualified Text.PrettyPrint.Mainland as PP
import qualified Text.PrettyPrint.Mainland.Class as PP

import           Abide.CTypes
import           Abide.Types
import           Abide.Types.Arch.X86_64
import qualified Abide.Types.Arch.PPC64 as P64

import           TestTypes

--------------------------------------------------------------------------------
-- Entry point

-- | Generate a test case for a given list of function parameters.
doCTest
  :: ( TestableArch arch abi
     , OutSymbol arch abi ~ reg
     , IsFPReg reg
     )
  => proxy (arch, abi) -> FnParamSpec -> IO T.Text
doCTest px ps = compileWith px binName (mkCGenerator px ps) (T.unlines $ mkASM px)

--------------------------------------------------------------------------------
-- C Generation

-- | A fixed name for the function that will be generated and called in any
-- given test case.
calledFnName :: T.Text
calledFnName = "foo"

-- | An infinite list of unique parameter names, used sequentially when
-- generating functions with parameter lists.
paramNames :: [T.Text]
paramNames = map (\n -> T.pack ('p' : show n)) [1..]

-- | In order to store particular bytes in a floating point value, we need to
-- memcpy them.  This integer variable name is used throughout a test program
-- to temporarily store the value we want to copy.
memCpyInt :: T.Text
memCpyInt = "i"

-- | Generate the LangC code for a particular function specification.
mkCGenerator
  :: ( TestableArch arch abi
     , OutSymbol arch abi ~ reg
     , IsFPReg reg
     )
  => proxy (arch, abi) -> FnParamSpec -> [C.Definition]
mkCGenerator px ps =
  mkIncludes ++
  mkGlobals px ps ++
  [ mkMainFn px ps calledFnName ]

-- | Generate a fixed set of include files for our generated C test cases.
mkIncludes :: [C.Definition]
mkIncludes =
  [ [C.cedecl|$esc:("#include <stdio.h>")|]
  , [C.cedecl|$esc:("#include <stdint.h>")|]
  , [C.cedecl|$esc:("#include <string.h>")|]
  ]

-- | We have, at least, the declaration for the extern function (which will be
-- the assembly code) and the global array of values, which will get filled by
-- the assembly code for printing.
mkGlobals
  :: ( TestableArch arch abi )
  => proxy (arch, abi) -> FnParamSpec -> [C.Definition]
mkGlobals px ps = [ mkExternFnDec ps, mkRegArray px, mkMemArray px]

-- | Declare the extern function (i.e., the assembly code function) we will
-- call.  The symbol name has to be right and so does the parameter list.
-- This will trick the compiler into putting the parameters into the locations
-- expected according to the ABI.
mkExternFnDec :: FnParamSpec -> C.Definition
mkExternFnDec ps =
  let pars = mkDecParamList ps
  in [C.cedecl|extern void $id:(calledFnName) ($params:(pars));|]

-- | A variable name for the global array of values.
globalRegArrayName :: T.Text
globalRegArrayName = "gRegs"

globalParArrayName :: T.Text
globalParArrayName = "gPars"

-- | A length for the global stack memory array.
globalParArrayLen :: Int
globalParArrayLen = 64

-- | Declare the global array, which should be an array of values
-- appropriately sized for the architecture.
mkRegArray
  :: ( TestableArch arch abi )
  => proxy (arch, abi) -> C.Definition
mkRegArray px =
  [C.cedecl|$ty:(convertCType (regSize px)) $id:(globalRegArrayName) [$int:(numRegs px)];|]

mkMemArray
  :: ( TestableArch arch abi )
  => proxy (arch, abi) -> C.Definition
mkMemArray px =
  [C.cedecl|$ty:(convertCType (regSize px)) $id:(globalParArrayName) [$(globalParArrayLen)];|]

-- | For a function declaration, convert the internal representation of a
-- parameter list to the corresponding LangC definitions.
mkDecParamList :: FnParamSpec -> [C.Param]
mkDecParamList ps = zipWith mkDecParam (map fst ps) paramNames
  where
    mkDecParam :: CType -> T.Text -> C.Param
    mkDecParam tp nm = [C.cparam|$ty:(convertCType tp) $id:(nm)|]

-- | Convert an Abide representation of a type to a LangC one.
convertCType :: CType -> C.Type
convertCType CInt8   = [C.cty|typename int8_t|]
convertCType CInt16  = [C.cty|typename int16_t|]
convertCType CInt32  = [C.cty|typename int32_t|]
convertCType CInt64  = [C.cty|typename int64_t|]
convertCType CFloat  = [C.cty|float|]
convertCType CDouble = [C.cty|double|]

-- | Generate the main function, which will generally just set up the
-- parameters to call one function and return.
mkMainFn
  :: TestableArch arch abi
  => proxy (arch, abi) -> FnParamSpec -> T.Text -> C.Definition
mkMainFn px ps nm =
  let parDefs = [C.citems|$items:(mkParamDefs ps)|]
      clear   = [C.citem|$id:(memCpyInt) = 0;|]
      fnCall  = [C.citem|$item:(mkFnCall ps)|]
      ret     = [C.citem|return 0;|]
      prints  = zipWith (printRegVar px) (regStrings px) [0..] ++
                concatMap (printMem px) ps
      main    = [C.cfun|int main() { $items:(parDefs ++ [clear, fnCall] ++ prints ++ [ret]) }|]
  in [C.cedecl|$func:(main)|]

-- | First we declare an int which we will use to memcpy bits into floating
-- point variables, and then we handle all of the remaining parameters.
mkParamDefs :: FnParamSpec -> [C.BlockItem]
mkParamDefs ps =
  [C.citem|int $id:(memCpyInt);|] :
  concat (zipWith mkParamDef paramNames ps)

-- | Declare and define one parameter, given a name for the variable, a type,
-- and a value.
mkParamDef :: C.ToExp a => T.Text -> (CType, a) -> [C.BlockItem]
mkParamDef nm (CFloat, val) =
  let intVal = [C.citem|$id:(memCpyInt) = $val;|]
      floatDec = [C.citem|float $id:(nm);|]
  in [intVal, floatDec, mkMemCpy nm]
mkParamDef nm (CDouble, val) = undefined
mkParamDef nm (t, val) = [[C.citem|$ty:(convertCType t) $id:(nm) = $exp:(val);|]]

-- | For float/double values we add the memcpy machinery.
mkMemCpy :: T.Text -> C.BlockItem
mkMemCpy nm = [C.citem|memcpy(&$id:(nm), &$id:(memCpyInt), sizeof($id:(nm)));|]

-- | Generate the code to call the generated function, with a specific number
-- of parameters which should already be defined.
mkFnCall :: FnParamSpec -> C.BlockItem
mkFnCall ps = [C.citem|$id:(calledFnName)($args:(mkArgList ps));|]

-- | The argument list for calling the generated function is just a sequence
-- of variable names.
mkArgList :: FnParamSpec -> [C.Exp]
mkArgList ps = take (length ps) (map mkArg paramNames)
  where
    mkArg nm = [C.cexp|$id:(nm)|]

--------------------------------------------------------------------------------
-- Printing of results

-- | Generate a printf statement that prints the register contents.
printRegVar :: TestableArch arch abi => proxy (arch, abi) -> T.Text -> Int -> C.BlockItem
printRegVar px reg off =
  let pfstr = T.unpack reg ++ " %lx\n"
  in [C.citem|printf($string:(pfstr), $id:(globalRegArrayName)[$(off)]);|]

-- | Generate a loop that clamps the global array values to the right size
-- type, and then prints the values.
printMem :: TestableArch arch abi => proxy (arch, abi) -> (CType, Word64) -> [C.BlockItem]
printMem px (ct, w) =
  let loopPrintStr = "offset %d " ++ ctypePrintfSpec ct ++ "\n"
  in [ [C.citem|for(int $id:(loopVarName) = 0;
                    $id:(loopVarName) < $int:(globalParArrayLen);
                    $id:(loopVarName)++)
                    { $ty:(convertCType (floatToInt ct)) $id:(sizedMemVarName) =
                        ($ty:(convertCType (floatToInt ct))) $id:(globalParArrayName)[$id:(loopVarName)];
                      printf($string:(loopPrintStr),
                             ($id:(loopVarName) + 1) * 8,
                             $id:(sizedMemVarName)); }|]
     , [C.citem|printf("parameter div\n");|]]

-- | We use `movq` to get 64 bits of memory at a time, but when we compare for
-- the values we want, we sometimes need to look at fewer bits.  These
-- variables will hold the relevant bits.
sizedMemVarName :: T.Text
sizedMemVarName = "lowbits"

-- | The known name of the loop counter used to iterate over the stack.
loopVarName :: T.Text
loopVarName = "offset"

-- | For a given C type, return the printf format specifier that goes with it.
-- Hex is probably easiest to work with.  TODO: floats/doubles are currently
-- broken.  We need to read the bits into ints, rather than using floats.
ctypePrintfSpec :: CType -> String
ctypePrintfSpec CInt8   = "%x"
ctypePrintfSpec CInt16  = "%x"
ctypePrintfSpec CInt32  = "%x"
ctypePrintfSpec CInt64  = "%lx"
ctypePrintfSpec CFloat  = "%x"
ctypePrintfSpec CDouble = "%x"

-- | Even when a parameter is a floating point type, we want to compare it as
-- an integer so we can just see the bits more easily.
floatToInt :: CType -> CType
floatToInt CFloat = CInt32
floatToInt CDouble = CInt64
floatToInt t = t

--------------------------------------------------------------------------------
-- The code that wraps up some embedded assembly in order to check the
-- contents of registers and the stack is all generated below here.

-- | The architecture-independent structure of the assembly generation, which
-- calls into the architecture-specific stuff.
mkASM :: TestableArch arch abi => proxy (arch, abi) -> [T.Text]
mkASM p =
  mkAsmHeader p ++
  mkRegHeader p ++
  concat (zipWith (mkRegAsm p) (regVarNames p) (map (* 8) [0..])) ++
  mkMemHeader p ++
  concatMap (mkMemAsm p . (* 8)) [0..(globalParArrayLen - 1)] ++
  mkAsmFooter p

-- | The code that needs to go at the top of the asm file for X64
mkX64AsmHeader :: [T.Text]
mkX64AsmHeader =
  [ ".intel_syntax noprefix"
  , ".section .text"
  , ".globl foo"
  , ".type foo, @function"
  , "foo:"
  ]

-- | The code that needs to go at the top of the asm file for PPC64
mkPPC64AsmHeader :: [T.Text]
mkPPC64AsmHeader =
  [ "\t.section \".toc\", \"aw\""
  , ".LC0:"
  , "\t.quad " <> globalRegArrayName
  , ".LC1:"
  , "\t.quad " <> globalParArrayName
  , "\t.section \".text\""
  , "\t.globl " <> calledFnName
  , "\t.type " <> calledFnName <> ", @function"
  , calledFnName <> ":"
  ]

-- | On X64 we can just movq everything, regardless of type
mkX64RegAsm :: (X86_64Registers, T.Text) -> Int -> [T.Text]
mkX64RegAsm (_, nm) off =
  [ "\tmovq [" <> globalRegArrayName <> "+" <> T.pack (show off) <> "], " <> nm ]

mkPPC64RegHeader :: [T.Text]
mkPPC64RegHeader =
  [ "\taddis 14, 2,.LC0@toc@ha"
  , "\tld 14, .LC0@toc@l(14)"
  ]

mkPPC64RegAsm :: (P64.PPC64Registers, T.Text) -> Int -> [T.Text]
mkPPC64RegAsm (reg, nm) off =
  if isFPReg reg
  then ["\tstfs " <> trailingDecimal nm <> ", " <> T.pack (show off) <> "(14)"]
  else ["\tstd "  <> trailingDecimal nm <> ", " <> T.pack (show off) <> "(14)"]

trailingDecimal :: T.Text -> T.Text
trailingDecimal = T.dropWhile isAlpha

-- | We can't have two memory accesses in a single movq, so use R10 as an
-- intermediate store.  R10 never holds parameters, so that should be fine.
mkX64MemAsm :: Int -> [T.Text]
mkX64MemAsm n =
  let ntxt = T.pack (show n)
  in [ "\tmovq R10, [RSP+" <> ntxt <> "]"
     , "\tmovq [" <> globalParArrayName <> "+" <> ntxt <> "], R10"
     ]

mkPPC64MemHeader :: [T.Text]
mkPPC64MemHeader =
  [ "\taddis 14, 2,.LC1@toc@ha"
  , "\tld 14, .LC1@toc@l(14)"
  ]

mkPPC64MemAsm :: Int -> [T.Text]
mkPPC64MemAsm n =
  [ "\tld 3, " <> T.pack (show n) <> "(1)"
  , "\tstd 3, " <> T.pack (show n) <> "(14)"
  ]

--------------------------------------------------------------------------------
-- Compilation stuff below here

binName :: FP.FilePath
binName = "abide-test.exe"

-- | Do all of the file writing and compilation.  First we write the C and
-- assembly to temporary locations, then we compile those into an executable,
-- and if things are still going according to plan we run the executable and
-- pass along the output for parsing.
compileWith
  :: TestableArch arch abi
  => proxy (arch, abi) -> FP.FilePath -> [C.Definition] -> T.Text -> IO T.Text
compileWith p bin code asm =
  Tmp.withSystemTempDirectory "compile-test" $ \dir ->
    Tmp.withSystemTempFile "main.c" $ \fpc hc ->
      Tmp.withSystemTempFile "foo.S" $ \fps hs -> do
        PP.hPutDocLn hc (PP.ppr code) >> IO.hFlush hc
        T.hPutStrLn hs asm >> IO.hFlush hs
        let defaultFlags = ["-static", "-O", "-no-pie", fpc, fps, "-o", dir </> bin]
        (ec, _, cerr) <- Proc.readProcessWithExitCode (ccFP p) (ccFlags p ++ defaultFlags) ""
        case ec of
          SE.ExitSuccess -> do
            let (exe, args) = exeWrapper p (dir </> bin)
            (cec, cout, cerr) <- Proc.readProcessWithExitCode (dir </> bin) args ""
            case cec of
              SE.ExitSuccess -> do
                -- writeFile "main.ok.c" (PP.pretty 120 (PP.ppr code))
                -- writeFile "foo.ok.s" (T.unpack asm)
                return $ T.pack cout
              SE.ExitFailure _ -> do
                -- writeFile "main.c.runbad" (PP.pretty 120 (PP.ppr code))
                -- writeFile "foo.s.runbad" (T.unpack asm)
                error $ show cerr
          SE.ExitFailure n -> do
            -- writeFile "main.c.ccbad" (PP.pretty 120 (PP.ppr code))
            -- writeFile "foo.s.ccbad" (T.unpack asm)
            error $ show cerr

--------------------------------------------------------------------------------

instance C.ToIdent T.Text where
  toIdent t = C.toIdent (T.unpack t)
