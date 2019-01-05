{-# LANGUAGE QuasiQuotes #-}

module TestGenerator where

import Language.C.Quote as C
import Language.C.Quote.C as C

import Abide.CTypes

import TestTypes

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
mkCalledFn paramspec name =
  let ps = mkDecParamList paramspec
      fn = [C.cfun|void $id:(name) ($params:(ps)) { return; } |]
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

-- TODO: Generate a declaration for each parameter, setting it to a unique
-- value.

-- | Generate the main function, which will generally just set up the
-- parameters to call one function and return.
mkMainFn :: FnParamSpec -> String -> C.Definition
mkMainFn ps nm =
  let fnCall = [C.citem|$item:(mkFnCall ps nm)|]
      ret =    [C.citem|return 0;|]
      main = [C.cfun|int main() { $items:([fnCall,ret]) } |]
  in [C.cedecl|$func:(main)|]

mkFnCall :: FnParamSpec -> String -> C.BlockItem
mkFnCall ps nm = undefined -- [C.cstm|$id:(nm)($args:(mkArgList ps))|]
