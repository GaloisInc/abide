{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module : Abide.Compute
Copyright : (c) 2018 Galois Inc.
License : BSD3 (see LICENSE)

This module is responsible for doing the actual transducing on a finite state
transducer, as well as for getting at the specific FSTs for an
architecture/ABI pair.
-}

module Abide.Compute
  ( computeParam
  ) where

import           Control.Lens ( (^.) )
import           Data.List ( find )
import           Data.Maybe ( fromJust, fromMaybe )
import           Data.Parameterized.NatRepr
import           Data.Proxy
import qualified Data.Map.Strict as M
import qualified Data.Tuple.Select as TS
import           Numeric.Natural

import           Abide.CTypes
import qualified Abide.Parse as P
import           Abide.Types

computeParam
  :: ( ParamABI arch abi
     , CTypeInput arch abi
     , InSymbol arch abi ~ i
     , OutSymbol arch abi ~ o
     , Eq i
     , IsStack o
     , ArchParamBaseOffset arch
     )
  => proxy (arch, abi)
  -> [CType]
  -> Either o StackOffset
computeParam proxy
  = transduce paramFST . map
      (\ctype ->
        ( ctypeInputClass proxy ctype
        , ctypeInputSize proxy ctype
        ))

transduce :: forall arch abi i o.
             ( InSymbol arch abi ~ i
             , OutSymbol arch abi ~ o
             , Eq i
             , IsStack o
             , ArchParamBaseOffset arch
             )
          => FST arch abi -> [(i, Natural)] -> Either o StackOffset
transduce fst inputs =
  let (reg, so) = go fst 1 0 inputs []
  in if isStack reg
     then Right (so + paramBaseOffset (Proxy @arch))
     else Left reg
    where
      go _ _ _ [] outs = head outs
      go fst n offset (i : is) outs =
        let (nn, (o, off)) = traverseEdge fst n i offset
        in go fst nn off is ((o, off) : outs)

-- A lot of unsafe stuff?  Maybe use types to guarantee presence 
traverseEdge :: forall arch abi i o.
                ( InSymbol arch abi ~ i
                , OutSymbol arch abi ~ o
                , Eq i
                , IsStack o
                , ArchParamBaseOffset arch
                )
             => FST arch abi
             -> UID
             -- ^ The current node UID, to find outgoing edges
             -> (i, StackOffset)
             -- ^ The input symbol and its potential size if placed on the stack
             -> StackOffset
             -- ^ The stack offset so far
             -> (UID, (o, StackOffset))
traverseEdge fst n (i, size) currOffset =
  let edges     = fromMaybe (error "edge lookup failed in abide")
                            (M.lookup n (fst ^. nodeMap))
      (Just e)  = find ((== i) . (^. inSymbol)) edges
      newOffset = if incrementsStack (e ^. outSymbol)
                  then size + currOffset
                  else currOffset
  in (e ^. dst, (e ^. outSymbol, newOffset))
