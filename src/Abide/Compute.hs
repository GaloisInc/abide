{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module : Abide.Compute
Copyright : (c) 2018 Galois Inc.
License : BSD3 (see LICENSE)

This module is responsible for doing the actual transducing on a finite state
transducer, as well as for getting at the specific FSTs for an
architecture/ABI pair.
-}

module Abide.Compute where

import           Control.Lens ( (^.) )
import           Data.List ( find )
import           Data.Maybe ( fromJust, fromMaybe )
import           Data.Parameterized.Some ( Some(..) )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Map.Strict as M
import qualified Data.Tuple.Select as TS

import qualified Abide.Parse as P
import           Abide.Types



--------------------------------------------------------------------------------
-- Getting the right FST
class HasFST arch abi where
  getFST :: FST arch abi

instance HasFST X86_64 SystemV where
  getFST = P.x86_64FST

instance HasFST PPC SystemV where
  getFST = P.ppcFST

transduce :: forall arch abi i o.
             ( InSymbol arch abi ~ i, OutSymbol arch abi ~ o, Eq i)
          => FST arch abi -> [i] -> o
transduce fst is = go fst 1 is [] 
  where
    go fst _ [] os = head os
    go fst n (i:is) os =
      let (nn, o) = traverseEdge fst n i
      in go fst nn is (o:os)

-- A lot of unsafe stuff?  Maybe use types to guarantee presence 
traverseEdge :: forall arch abi i o.
                ( InSymbol arch abi ~ i, OutSymbol arch abi ~ o, Eq i)
             => FST arch abi -> UID -> i -> (UID, o)
traverseEdge fst n i = let edges = fromMaybe (error "edge lookup failed in abide")
                                             (M.lookup n (fst ^. nodeMap))
                           (Just e) = find ((== i) . (^. inSymbol)) edges
                       in (e ^. dst, e ^. outSymbol)
                       
