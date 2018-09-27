{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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


--------------------------------------------------------------------------------
-- We need to handle FP registers a special way sometimes, so define a class
-- to check that

type family ArchRegs arch = regs

type instance ArchRegs X86_64 = X64.X86_64Registers
type instance ArchRegs PPC = PPC.PPCRegisters

class IsFPReg reg where
  isFPReg :: reg -> Bool

instance IsFPReg X64.X86_64Registers where
  isFPReg X64.MMX0 = True
  isFPReg X64.MMX1 = True
  isFPReg X64.MMX2 = True
  isFPReg X64.MMX3 = True
  isFPReg X64.MMX4 = True
  isFPReg X64.MMX5 = True
  isFPReg X64.MMX6 = True
  isFPReg X64.MMX7 = True
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