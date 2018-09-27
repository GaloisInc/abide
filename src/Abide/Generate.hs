{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module : Abide.Generate
Copyright : (c) 2018 Galois Inc.
License : BSD3 (see LICENSE)

This module is responsible for generating FSTs and, when necessary, writing
them to files in an OpenFST format.  The abstraction that will (hopefully)
generalize all the different architecture and ABI pairings is essentially a
mapping from input symbols to sequences of output symbols.  So if we have a
class of parameters for integer parameters called INTEGER, the input for FST
generation might be `(INTEGER, [Register1, Register2, IntStack])` where the
first two integer parameters are assigned to the two registers, and all
remaining parameters are assigned to the stack.

Note that correct handling of the stack is work in progress.
-}

module Abide.Generate
  ( mkFSTGen
  , generateFiles
  ) where

import           Control.Comonad.Store.Class ( peeks )
import           Control.Lens ( makeLenses, (^.) )
import           Control.Lens.Traversal ( holesOf )
import           Control.Monad ( forM_ )
import           Data.List ( nub, unwords )
import qualified Data.Map.Strict as M
import           Data.Maybe ( fromJust )
import           Data.Semigroup ( (<>) )
import qualified Data.Set as S
import qualified Data.Universe as U
import qualified System.IO as Sys

import           Abide.Types
import           Abide.Types.Arch.X86_64

import           Prelude

--------------------------------------------------------------------------------
-- FSTGenerator construction

-- | The main entry point for creating a finite state transducer.  Once
-- generated, it is used to generate the OpenFST files, which in turn can be
-- read in to actually compute how a particular argument is passed to a
-- function.
--
-- Takes a `TypeMap`, which is a mapping from values in the input language
-- (this is usually classes of types, such as integers or floating-point
-- values) to stacks of values in the output language (which is usually going
-- to be registers).  For example, on X86_64 the first 6 integer-class values
-- are passed in a specific registers, so the input to this function would
-- include a mapping like `IntegerClass \mapsto [RDI, RSI, ...]`.
mkFSTGen :: forall arch abi i o.
            (InSymbol arch abi ~ i, OutSymbol arch abi ~ o, Ord o, Eq i)
         => TypeMap i o -> FSTGenerator arch abi
mkFSTGen tm = FSTGenerator tm edges states
  where
    states = mkStates tm
    edges = nub $ mkEdges tm states

-- Generates new `State` values, relying on an infinite list of positive
-- integers for UIDs.  The `FSTGenerator` needs a `State` for each possible combination
-- of register allocations, and so we compute a product of the output labels,
-- since we those will correspond to our registers.
mkStates :: (Ord o) => TypeMap i o -> [State i o]
mkStates tm = zipWith State [1..] bases
  where
    bases = map S.fromList $ sequence (M.elems tm)

-- Creating new edges is done by: 1) computing which states can be reached
-- from a particular state, and 2) generating edges for each (including
-- single-state loops).
mkEdges :: (Ord o, Eq i)
        => TypeMap i o -> [State i o] -> [Edge (State i o) i o]
mkEdges tm sts =
  concatMap (\src -> concatMap (pairEdges tm src) (nextStates src sts tm)) sts

-- Generate edges for a particular pair of states.  This could produce more
-- than one edge when, e.g. several kinds of registers have been used up and
-- so both corresponding types just keep getting stack-allocated without
-- changing the state.
pairEdges :: (Ord o, Eq i)
          => TypeMap i o -> State i o -> State i o -> [Edge (State i o) i o]
pairEdges tm src dst
  -- loops
  | src == dst =
      case S.toList $ S.filter (terminalReg tm) (src ^. basis) of
        [] -> []
        os -> let ios = flatFst $ map (\o -> (findKeyFor tm o, o)) os
              in map (uncurry (Edge src dst)) ios
  -- distinct source and destination
  | otherwise  = map (uncurry (Edge src dst)) ios
  where
    os = S.toList $ S.difference (src ^. basis) (dst ^. basis)
    ios = flatFst $ map (\o -> (findKeyFor tm o, o)) os

-- Does the register (type `o`) parameter occur in the last spot of one of the
-- register stacks?  If so, it means we've exhausted the registers for that
-- type and so have a stack allocation or other terminal state.
terminalReg :: Eq o => TypeMap i o -> o -> Bool
terminalReg tm o = any ((== o) . last) tm

-- The `TypeMap` maps input labels to lists of registers, and we sometimes
-- need to find all those input labels which map to a particular register.
findKeyFor :: (Ord o) => TypeMap i o -> o -> [i]
findKeyFor tm v = [k | (k,vs) <- M.toList tm, v `elem` vs]

flatFst :: [([a],b)] -> [(a,b)]
flatFst [] = []
flatFst ((as,b):rest) = map (, b) as ++ flatFst rest

-- Compute the successor states for the input state.  This depends on the
-- correct instantiation of the `Next` class.
nextStates :: (Ord o)
           => State i o -> [State i o] -> TypeMap i o -> [State i o]
nextStates st all tm =
  -- have to convert to list because Set isn't a Functor, change this?
  let b = S.toList $ st ^. basis
      bs = map S.fromList $ doEach (nextReg tm) b
  in map (fromJust . lookupByBasis all) bs

-- Behavior is not defined if `o` occurs in more than one stack.  More awkward
-- `Just`s that should be avoidable.
nextReg :: Eq o => TypeMap i o -> o -> o
nextReg tm o | terminalReg tm o = o
nextReg tm o = findNext o . head $ filter (elem o) (M.elems tm)
  where
    findNext o (x:y:xs) | o == x    = y
                        | otherwise = findNext o (y:xs)
    findNext o (x:_) = x

lookupByBasis :: (Ord o)
              => [State i o] -> Basis o -> Maybe (State i o)
lookupByBasis [] _ = Nothing
lookupByBasis (s:ss) b | b == s ^. basis = Just s
                       | otherwise       = lookupByBasis ss b

doEach :: Traversable t => (s -> s) -> t s -> [t s]
doEach f = map (peeks f) . holesOf traverse

diffBases :: (Eq a) => [a] -> [a] -> [a]
diffBases (b1:bs1) (b2:bs2) | b1 == b2 = diffBases bs1 bs2
                            | otherwise = [b1]
diffBases _ _ = []


--------------------------------------------------------------------------------
-- File generation stuff

-- | This function generates the OpenFST format files for a particular FST.
-- Three files will be generated, namely the ASCII-format FST file, the input
-- symbol mapping, and the output symbol mapping.  It should then be possible
-- to compile the files to execute the FST as desired.
--
-- It expects a `FilePath` without any particular extension.  For example, if
-- you provide the the path `"/home/user/test"`, this function will generate
-- files such as `"/home/user/test.fst.txt"`, `"/home/user/test.isym"`, and so
-- on.
generateFiles :: forall arch abi i o.
                 ( InSymbol arch abi ~ i, OutSymbol arch abi ~ o
                 , U.Universe i, Show i
                 , U.Universe o, Show o)
              => FilePath -> FSTGenerator arch abi -> IO ()
generateFiles fp fst = do
  generateFSTFile (fp <> ".fst.txt") fst
  generateLabelFiles fp fst

generateFSTFile :: forall arch abi i o.
                   ( InSymbol arch abi ~ i, OutSymbol arch abi ~ o
                   , Show i, Show o)
                => FilePath -> FSTGenerator arch abi -> IO ()
generateFSTFile fp fst = Sys.withFile fp Sys.WriteMode $ \file ->
  forM_ (fst ^. edges) (Sys.hPutStrLn file . edgeFSTFormat)

edgeFSTFormat :: (Show i, Show o) => Edge (State i o) i o -> String
edgeFSTFormat e = unwords
  [ show $ e ^. src . uid
  , show $ e ^. dst . uid
  , show $ e ^. inSymbol
  , show $ e ^. outSymbol
  ]

generateLabelFiles :: forall arch abi i o.
                      ( InSymbol arch abi ~ i, OutSymbol arch abi ~ o
                      , U.Universe i, Show i
                      , U.Universe o, Show o)
                   => FilePath -> FSTGenerator arch abi -> IO ()
generateLabelFiles fp fst = do
  generateInputFile  (fp <> ".isym") fst
  generateOutputFile (fp <> ".osym") fst

generateInputFile :: forall arch abi i o.
                     (InSymbol arch abi ~ i, U.Universe i, Show i)
                  => FilePath -> FSTGenerator arch abi -> IO ()
generateInputFile fp fst = Sys.withFile fp Sys.WriteMode $ \file ->
  forM_ (zip (U.universe :: [i]) [1..])
        (\(oval, n) -> Sys.hPutStrLn file (show oval <> " " <> show n))

generateOutputFile :: forall arch abi i o.
                      (OutSymbol arch abi ~ o, U.Universe o, Show o)
                   => FilePath -> FSTGenerator arch abi -> IO ()
generateOutputFile fp fst = Sys.withFile fp Sys.WriteMode $ \file ->
  forM_ (zip (U.universe :: [o]) [1..])
        (\(oval, n) -> Sys.hPutStrLn file (show oval <> " " <> show n))
