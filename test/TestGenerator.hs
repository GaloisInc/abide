{-# LANGUAGE QuasiQuotes #-}

module TestGenerator where

import           Language.C.Pretty ()
import qualified Language.C.Quote as C
import qualified Language.C.Quote.C as C
import qualified Text.PrettyPrint.Mainland as PP
import qualified Text.PrettyPrint.Mainland.Class as PP

import           Abide.CTypes
import           Abide.Types.Arch.X86_64

import           TestTypes

data Test = Test
  { cGenerator :: CGenerator
  }

data CGenerator

-- | Generate a test case for a given list of function parameters.
mkTest :: FnParamSpec -> Test
mkTest = undefined

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
mkCGenerator :: FnParamSpec -> [C.Definition]
mkCGenerator ps =
  mkIncludes ++ [mkCalledFn ps calledFnName, mkMainFn ps calledFnName]

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
mkCalledFn :: FnParamSpec -> String -> C.Definition
mkCalledFn pspec name =
  let ps = mkDecParamList pspec
      fn = [C.cfun|void $id:(name) ($params:(ps)) { $items:(inlineAsm pspec) return; } |]
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

mkParamDefs :: FnParamSpec -> [C.BlockItem]
mkParamDefs ps =
  [C.citem|typename int32_t $id:(memCpyInt);|] :
  (concat $ zipWith mkParamDef paramNames ps)

mkParamDef :: C.ToExp a => String -> (CType, a) -> [C.BlockItem]
mkParamDef nm (CFloat, val) =
  let intVal = [C.citem|$id:(memCpyInt) = $val;|]
      floatDec = [C.citem|float $id:(nm);|]
  in [intVal, floatDec, mkMemCpy nm]
mkParamDef nm (CDouble, val) = undefined
mkParamDef nm (t, val) = [[C.citem|$ty:(convertCType t) $id:(nm) = $exp:(val);|]]

mkMemCpy :: String -> C.BlockItem
mkMemCpy nm = [C.citem|memcpy(&$id:(nm), &$id:(memCpyInt), sizeof($id:(nm)));|]

mkFnCall :: FnParamSpec -> String -> C.BlockItem
mkFnCall ps nm = [C.citem|$id:(nm)($args:(mkArgList ps));|]

mkArgList :: FnParamSpec -> [C.Exp]
mkArgList ps = take (length ps) (map mkArg paramNames)
  where
    mkArg nm = [C.cexp|$id:(nm)|]

regVariables :: [(X86_64Registers, String)]
regVariables =
  [ (RDI,  "rdi")
  , (RSI,  "rsi")
  , (RDX,  "rdx")
  , (RCX,  "rcx")
  , (R8 ,  "r8")
  , (R9,   "r9")
  -- , (YMM0, "ymm0")
  -- , (YMM1, "ymm1")
  -- , (YMM2, "ymm2")
  -- , (YMM3, "ymm3")
  -- , (YMM4, "ymm4")
  -- , (YMM5, "ymm5")
  -- , (YMM6, "ymm6")
  -- , (YMM7, "ymm7")
  ]


--------------------------------------------------------------------------------
-- The code that wraps up some embedded assembly in order to check the
-- contents of registers and the stack is all generated below here.

inlineAsm :: FnParamSpec -> [C.BlockItem]
inlineAsm fns =
  let nms = map snd regVariables
  in map mkRegVarDecl nms ++
     map mkRegAsm nms ++
     map printRegVar nms
  --gpRegVariables ++ fpRegVariables ++ genAssembly

mkRegVarDecl :: String -> C.BlockItem
mkRegVarDecl nm = [C.citem|typename int64_t $id:(nm);|]

mkRegAsm :: String -> C.BlockItem
mkRegAsm nm =
  let asmString = "\"movq %%" ++ nm ++ ", %0;\" : \"=a\" (" ++ nm ++ ")"
  in [C.citem|__asm__( $esc:(asmString) );|]

printRegVar :: String -> C.BlockItem
printRegVar nm =
  let fstr = nm ++ " : %lu\n"
  in [C.citem|printf($(fstr), $id:(nm));|]
