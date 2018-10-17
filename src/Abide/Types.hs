{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}

{-|
Module : Abide.Types
Copyright : (c) 2018 Galois Inc.
License : BSD3 (see LICENSE)

This module defines the general types used throughout the library.  Types
specific to a particular ABI or architecture are nested within this module and
should always be imported qualified.
-}

module Abide.Types where

import           Control.Lens ( Lens', makeLenses, (^.) )
import qualified Data.Map.Strict as M
import qualified Data.Parameterized.Ctx as C
import qualified Data.Set as S
import           Numeric.Natural

import qualified Abide.Types.ABI.SystemV as SV
import qualified Abide.Types.Arch.PPC as PPC
import qualified Abide.Types.Arch.X86_64 as X64

--------------------------------------------------------------------------------
-- Types for parameterizing FSTs

data X86_64

data PPC

data SystemV

type family InSymbol arch abi = i | i -> arch abi

type instance InSymbol X86_64 SystemV = SV.X86_64Classes
type instance InSymbol PPC SystemV = SV.PPCClasses

type family OutSymbol arch abi = o | o -> arch abi

type instance OutSymbol X86_64 SystemV = X64.X86_64Registers
type instance OutSymbol PPC SystemV = PPC.PPCRegisters

--------------------------------------------------------------------------------
-- Specialized FST types for register allocation

data FSTGenerator arch abi where
  FSTGenerator :: forall arch abi i o. (InSymbol arch abi ~ i, OutSymbol arch abi ~ o)
               => TypeMap i o -> [Edge (State i o) i o] -> [State i o] -> FSTGenerator arch abi

-- manually define the lens for GADTs
typeMap :: forall arch abi i o.
           (InSymbol arch abi ~ i, OutSymbol arch abi ~ o)
        => Lens' (FSTGenerator arch abi) (TypeMap i o)
typeMap f (FSTGenerator tm es sts) = fmap (\tm' -> FSTGenerator tm' es sts) (f tm)

edges :: forall arch abi i o.
         (InSymbol arch abi ~ i, OutSymbol arch abi ~ o)
      => Lens' (FSTGenerator arch abi) [Edge (State i o) i o]
edges f (FSTGenerator tm es sts) = fmap (\es' -> FSTGenerator tm es' sts) (f es)

states :: forall arch abi i o.
          (InSymbol arch abi ~ i, OutSymbol arch abi ~ o)
       => Lens' (FSTGenerator arch abi) [State i o]
states f (FSTGenerator tm es sts) = fmap (FSTGenerator tm es) (f sts)


data Edge n i o = Edge
  { _src :: n
  , _dst :: n
  , _inSymbol  :: i
  , _outSymbol :: o
  } deriving (Eq)

-- Nodes in the FST are specialized for the ABI/calling conventions use case,
-- as they enforce that nodes are constructed from elements of the output
-- language (registers).
data State i o = State
  { _uid :: UID
  , _basis :: Basis o
  } deriving (Eq)

type TypeMap i o = M.Map i [o]

type Basis o = S.Set o

type UID = Int

makeLenses ''Edge
makeLenses ''State

--------------------------------------------------------------------------------
-- Type classes

instance (Show i, Show o) => Show (State i o) where
  show st = "state" ++ show (st ^. uid) ++ " " ++ show (S.toList (st ^. basis))

instance (Show n, Show i, Show o) => Show (Edge n i o) where
  show e = "edge " ++ unwords
    [ show (e ^. src)
    , show (e ^. dst)
    , show (e ^. inSymbol)
    , show (e ^. outSymbol)]

--------------------------------------------------------------------------------
-- Computation

-- Some of the information that is useful in the FST, such as the basis that
-- defines a state, is not possible to recover reasonably from a file.
-- Moreover, it is not interesting when transducing.  In fact, all we really
-- want is a mapping from node UIDs to outgoing edges.

newtype FST arch abi = FST { _nodeMap :: M.Map UID [Edge UID (InSymbol arch abi) (OutSymbol arch abi)] }

makeLenses ''FST

type StackOffset = Natural


--------------------------------------------------------------------------------
-- Type classes

-- This class just lets us get an FST for a particular arch/abi combo in a
-- generic way.
class HasFST arch abi where
  getFST :: FST arch abi

-- We need to differentiate between parameters placed in a register versus
-- those that are passed on the stack.  For now, we can't do this by type
-- alone, so we use a type class to say which constructors correspond to the
-- stack and which do not.
class IsStack reg where
  isStack :: reg -> Bool

instance IsStack X64.X86_64Registers where
  isStack X64.StackInt   = True
  isStack X64.StackFloat = True
  isStack X64.StackMem   = True
  isStack _              = False

instance IsStack PPC.PPCRegisters where
  isStack PPC.StackGP    = True
  isStack PPC.StackFloat = True
  isStack PPC.StackVec   = True
  isStack _              = False

--------------------------------------------------------------------------------
-- FP registers sometimes need special handling.

class IsFPReg reg where
  isFPReg :: reg -> Bool

instance IsFPReg X64.X86_64Registers where
  isFPReg X64.YMM0 = True
  isFPReg X64.YMM1 = True
  isFPReg X64.YMM2 = True
  isFPReg X64.YMM3 = True
  isFPReg X64.YMM4 = True
  isFPReg X64.YMM5 = True
  isFPReg X64.YMM6 = True
  isFPReg X64.YMM7 = True
  isFPReg _ = False

instance IsFPReg PPC.PPCRegisters where
  isFPReg PPC.F1 = True
  isFPReg PPC.F2 = True
  isFPReg PPC.F3 = True
  isFPReg PPC.F4 = True
  isFPReg PPC.F5 = True
  isFPReg PPC.F6 = True
  isFPReg PPC.F7 = True 
  isFPReg PPC.F8 = True 
  isFPReg PPC.F9 = True 
  isFPReg PPC.F10 = True 
  isFPReg PPC.F11 = True 
  isFPReg PPC.F12 = True 
  isFPReg PPC.F13 = True
  isFPReg _ = False

class ReturnABI arch abi where
  computeReturn :: InSymbol arch abi -> OutSymbol arch abi

instance ReturnABI X86_64 SystemV where
  computeReturn = \case
    SV.INTEGER -> X64.RAX
    SV.SSE -> X64.YMM0
    c -> error $ "unsuported x86_64 class:" ++ show c

instance ReturnABI PPC SystemV where
  computeReturn = \case
    SV.PPCGP -> PPC.R3
    SV.PPCFLOAT-> PPC.F1

