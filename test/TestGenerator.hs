{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module TestGenerator where

import           Data.List ( intersperse )
import qualified Data.Text as T
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

import           TestTypes

import TestParams

-- | Generate a test case for a given list of function parameters.
doCTest
  :: ( TestableArch arch abi
     , OutSymbol arch abi ~ reg
     , IsFPReg reg
     )
  => proxy (arch, abi) -> FnParamSpec -> IO T.Text
doCTest px ps = compileWith (gccFP px) binName $ mkCGenerator px ps

--------------------------------------------------------------------------------
-- C Generation

-- | A fixed name for the function that will be generated and called in any
-- given test case.
calledFnName = "foo"

-- | An infinite list of unique parameter names, used sequentially when
-- generating functions with parameter lists.
paramNames = map (\n -> 'p' : show n) [1..]
-- | In order to store particular bytes in a floating point value, we need to
-- memcpy them.  This integer variable name is used throughout a test program
-- to temporarily store the value we want to copy.
memCpyInt = "i"

-- | Generate the LangC code for a particular function specification.
mkCGenerator
  :: ( TestableArch arch abi
     , OutSymbol arch abi ~ reg
     , IsFPReg reg
     )
  => proxy (arch, abi) -> FnParamSpec -> [C.Definition]
mkCGenerator px ps =
  mkIncludes ++ [mkCalledFn px ps calledFnName, mkMainFn ps calledFnName]

-- | Generate a fixed set of include files for our generated C test cases.
mkIncludes :: [C.Definition]
mkIncludes =
  [ [C.cedecl|$esc:("#include <stdio.h>")|]
  , [C.cedecl|$esc:("#include <stdint.h>")|]
  , [C.cedecl|$esc:("#include <string.h>")|]
  ]

-- | Generate a function with a known name and list of parameters, which will
-- be called from the main function in order to examine the stack and
-- registers.
mkCalledFn
  :: ( TestableArch arch abi
     , OutSymbol arch abi ~ reg
     , IsFPReg reg
     )
  => proxy (arch, abi) -> FnParamSpec -> String -> C.Definition
mkCalledFn px pspec name =
  let ps = mkDecParamList pspec
      fn = [C.cfun|void $id:(name) ($params:(ps)) { $items:(inlineAsm px pspec) return; } |]
  in [C.cedecl|$func:(fn)|]

-- | For a function declaration, convert the internal representation of a
-- parameter list to the corresponding LangC definitions.
mkDecParamList :: FnParamSpec -> [C.Param]
mkDecParamList ps = zipWith mkDecParam (map fst ps) paramNames
  where
    mkDecParam :: CType -> String -> C.Param
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
mkMainFn :: FnParamSpec -> String -> C.Definition
mkMainFn ps nm =
  let parDefs = [C.citems|$items:(mkParamDefs ps)|]
      fnCall  = [C.citem|$item:(mkFnCall ps nm)|]
      ret     = [C.citem|return 0;|]
      main    = [C.cfun|int main() { $items:(parDefs ++ [fnCall,ret]) }|]
  in [C.cedecl|$func:(main)|]

-- | First we declare an int which we will use to memcpy bits into floating
-- point variables, and then we handle all of the remaining parameters.
mkParamDefs :: FnParamSpec -> [C.BlockItem]
mkParamDefs ps =
  [C.citem|typename int32_t $id:(memCpyInt);|] :
  concat (zipWith mkParamDef paramNames ps)

-- | Declare and define one parameter, given a name for the variable, a type,
-- and a value.
mkParamDef :: C.ToExp a => String -> (CType, a) -> [C.BlockItem]
mkParamDef nm (CFloat, val) =
  let intVal = [C.citem|$id:(memCpyInt) = $val;|]
      floatDec = [C.citem|float $id:(nm);|]
  in [intVal, floatDec, mkMemCpy nm]
mkParamDef nm (CDouble, val) = undefined
mkParamDef nm (t, val) = [[C.citem|$ty:(convertCType t) $id:(nm) = $exp:(val);|]]

-- | For float/double values we add the memcpy machinery.
mkMemCpy :: String -> C.BlockItem
mkMemCpy nm = [C.citem|memcpy(&$id:(nm), &$id:(memCpyInt), sizeof($id:(nm)));|]

-- | Generate the code to call the generated function, with a specific number
-- of parameters which should already be defined.
mkFnCall :: FnParamSpec -> String -> C.BlockItem
mkFnCall ps nm = [C.citem|$id:(nm)($args:(mkArgList ps));|]

-- | The argument list for calling the generated function is just a sequence
-- of variable names.
mkArgList :: FnParamSpec -> [C.Exp]
mkArgList ps = take (length ps) (map mkArg paramNames)
  where
    mkArg nm = [C.cexp|$id:(nm)|]

--------------------------------------------------------------------------------
-- The code that wraps up some embedded assembly in order to check the
-- contents of registers and the stack is all generated below here.

-- | The main entry point for generating all of the inline assembly that
-- inspects the registers and stack memory.
inlineAsm
  :: ( TestableArch arch abi
     , OutSymbol arch abi ~ reg
     , IsFPReg reg
     )
  => proxy (arch, abi) -> FnParamSpec -> [C.BlockItem]
inlineAsm p fns =
  let nms = map snd (regVarNames p)
  in map mkRegVarDecl nms ++
     map mkRegAsm (regVarNames p) ++
     map printRegVar nms ++
     zipWith memVarDecl (map fst fns) memVarNames ++
     concat (zipWith3 mkStackAsm (map fst fns) memVarNames sizedMemVarNames)

-- | Declare a variable for a register.
mkRegVarDecl :: T.Text -> C.BlockItem
mkRegVarDecl nm = [C.citem|typename int64_t $id:(T.unpack nm);|]

mkRegAsm :: (IsFPReg reg) => (reg, T.Text) -> C.BlockItem
mkRegAsm (reg, t) = if isFPReg reg
                    then mkRegAsmFloat t
                    else mkRegAsmInt t

-- | Generate inline assembly for extracting a register value into a C
-- variable, specifically for integer class registers.
mkRegAsmInt :: T.Text -> C.BlockItem
mkRegAsmInt nm =
  let asmString = "\"movq %%" ++ T.unpack nm ++ ", %0;\" : \"=a\" (" ++ T.unpack nm ++ ")"
  in [C.citem|__asm__( $esc:(asmString) );|]

-- | Generate inline assembly for extracting a floating-point register value
-- into a C variable.
mkRegAsmFloat :: T.Text -> C.BlockItem
mkRegAsmFloat nm =
  let asmString = "\"movapd %%" ++ T.unpack nm ++ ", %0;\" : \"=x\" (" ++ T.unpack nm ++ ")"
  in [C.citem|__asm__( $esc:(asmString) );|]


-- | Generate a printf statement that prints the register contents.
printRegVar :: T.Text -> C.BlockItem
printRegVar nm =
  let fstr = T.unpack nm ++ " : %lx\n"
  in [C.citem|printf($(fstr), $id:(T.unpack nm));|]

-- | Known variable names used to read the stack.  We use one for each
-- parameter since they could be different sizes.
memVarNames = map (\x -> "memline" ++ show x) [0..]

-- | We use `movq` to get 64 bits of memory at a time, but when we compare for
-- the values we want, we sometimes need to look at fewer bits.  These
-- variables will hold the relevant bits.
sizedMemVarNames = map (\x -> "lowbits" ++ show x) [0..]

-- | The known name of the loop counter used to iterate over the stack.
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

floatToInt :: CType -> CType
floatToInt CFloat = CInt32
floatToInt CDouble = CInt64
floatToInt t = t

-- | Declare the variables used to read pieces of memory from the stack.
memVarDecl :: CType -> String -> C.BlockItem
memVarDecl ct nm = [C.citem|typename int64_t $id:(nm); |]

-- | Generate the inline assembly that reads from the stack.  We do this once
-- per parameter even though it's inefficient, because iterating over the
-- stack with different byte increments matching the parameter we are looking
-- for makes this much easier.  We also read more bytes than is generally
-- needed, because generating conditional assembly is more work.
--
-- The general idea is that we use the C loop iterator variable as an offset
-- to the stack pointer, and read that chunk of memory into a C variable.
--
-- Ideas for improvements are very welcome.
mkStackAsm :: CType -> String -> String -> [C.BlockItem]
mkStackAsm ct nm pfnm =
  let sz :: Int
      sz = fromIntegral $ ctypeByteSize ct
      loopASMStr =
        "\"movq (%%rbp, %1), %%r10\\n\\tmovq %%r10, %0\\n\" " ++
        ": \"=a\"(" ++ nm ++ ") " ++
        ": \"a\"(" ++ loopVarName ++ ") : \"r10\""
      loopASM = [C.citem|__asm__( $esc:(loopASMStr) );|]
      loopBody = loopASM : mkLoopPrintf ct nm pfnm
  in [ [C.citem|$ty:(convertCType (floatToInt ct)) $id:(pfnm);|]
     , [C.citem|for(typename uint64_t $id:(loopVarName)=0;
                $id:(loopVarName) <= 256;
                $id:(loopVarName) += $(sz))
                { $items:(loopBody) }|]
     , [C.citem|printf("parameter div\n");|]]

-- | The printing inside the loop body is a bit complicated.  Because we are
-- using `movq`, we end up with 64 bits.  However, if we are looking for a
-- magic value that is less than 64 bits, we can only compare against the
-- relevant part.  So before printing, we need to take the least significant
-- bits.
mkLoopPrintf :: CType -> String -> String -> [C.BlockItem]
mkLoopPrintf ct nm pfnm =
  let loopPrintStr = "offset %ld " ++ ctypePrintfSpec (floatToInt ct) ++ "\n"
  in [ [C.citem|$id:(pfnm) = ($ty:(convertCType (floatToInt ct)))$id:(nm);|]
     , [C.citem|printf($string:(loopPrintStr), $id:(loopVarName), $id:(pfnm));|]
     ]

--------------------------------------------------------------------------------
-- Compilation stuff below here

-- Some of these global names and flags will be made part of an
-- architecture-based type class soon.
ccFP = "gcc"

binName = "test.exe"

mkCCFlags code exe = ["-O0", "-o", exe, code]

compileWith :: FP.FilePath -> FP.FilePath -> [C.Definition] -> IO T.Text
compileWith cc bin code = Tmp.withSystemTempDirectory "compile-test" $ \dir ->
  Tmp.withSystemTempFile "test.c" $ \fp h -> do
    PP.hPutDocLn h $ PP.ppr code
    IO.hFlush h
    (ec, _, cerr) <- Proc.readProcessWithExitCode cc (mkCCFlags fp (dir </> bin)) ""
    case ec of
      SE.ExitSuccess -> do
        (_, cout, _) <- Proc.readProcessWithExitCode (dir </> bin) [] ""
        return $ T.pack cout
      SE.ExitFailure n -> do
        error $ show cerr
