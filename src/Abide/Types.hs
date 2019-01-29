{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
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
import           Data.Proxy
import           Data.Parameterized.NatRepr
import qualified Data.Text as T
import qualified Data.Set as S
import           GHC.TypeLits
import           Numeric.Natural
import qualified Text.Megaparsec as MP

import           Abide.CTypes
import qualified Abide.Types.ABI.SystemV as SV
import qualified Abide.Types.Arch.PPC32 as PPC32
import qualified Abide.Types.Arch.PPC64 as PPC64
import qualified Abide.Types.Arch.X86_64 as X64

--------------------------------------------------------------------------------
-- Types for parameterizing FSTs

data X86_64

data PPC32

data PPC64

data SystemV

type family InSymbol arch abi = i | i -> arch abi

type instance InSymbol X86_64 SystemV = SV.X86_64Classes
type instance InSymbol PPC32 SystemV = SV.PPC32Classes
type instance InSymbol PPC64 SystemV = SV.PPC64Classes

type family OutSymbol arch abi = o | o -> arch abi

type instance OutSymbol X86_64 SystemV = X64.X86_64Registers
type instance OutSymbol PPC32 SystemV = PPC32.PPC32Registers
type instance OutSymbol PPC64 SystemV = PPC64.PPC64Registers

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
class ParamABI arch abi where
  paramFST :: FST arch abi

class CTypeInput arch abi where
  ctypeInputClass
    :: proxy (arch, abi) -> CType -> InSymbol arch abi

  ctypeInputSize :: proxy (arch, abi) -> CType -> Natural

instance CTypeInput X86_64 SystemV where
  ctypeInputClass _ = \case
    CInt8   -> SV.INTEGER
    CInt16  -> SV.INTEGER
    CInt32  -> SV.INTEGER
    CInt64  -> SV.INTEGER
    CFloat  -> SV.SSE
    CDouble -> SV.SSE

  ctypeInputSize _ = \case
    CInt8   -> 8
    CInt16  -> 8
    CInt32  -> 8
    CInt64  -> 8
    CFloat  -> 8
    CDouble -> 8

instance CTypeInput PPC32 SystemV where
  ctypeInputClass _ = \case
    CInt8   -> SV.PPC32GP
    CInt16  -> SV.PPC32GP
    CInt32  -> SV.PPC32GP
    CInt64  -> error "unsupported int64 type on PPC32 SystemV."
    CFloat  -> SV.PPC32FLOAT
    CDouble -> SV.PPC32FLOAT

  ctypeInputSize _ = \case
    CInt8   -> 4
    CInt16  -> 4
    CInt32  -> 4
    CInt64  -> 8
    CFloat  -> 4
    CDouble -> 8

instance CTypeInput PPC64 SystemV where
  ctypeInputClass _ = \case
    CInt8   -> SV.PPC64GP
    CInt16  -> SV.PPC64GP
    CInt32  -> SV.PPC64GP
    CInt64  -> SV.PPC64GP
    CFloat  -> SV.PPC64FLOAT
    CDouble -> SV.PPC64FLOAT

  ctypeInputSize _ = \case
    CInt8   -> 8
    CInt16  -> 8
    CInt32  -> 8
    CInt64  -> 8
    CFloat  -> 8
    CDouble -> 8

-- We need to differentiate between parameters placed in a register versus
-- those that are passed on the stack.  For now, we can't do this by type
-- alone, so we use a type class to say which constructors correspond to the
-- stack and which do not.
class IsStack reg where
  isStack :: reg -> Bool
  incrementsStack :: reg -> Bool

instance IsStack X64.X86_64Registers where
  isStack X64.StackInt   = True
  isStack X64.StackFloat = True
  isStack X64.StackMem   = True
  isStack _              = False
  incrementsStack = isStack


instance IsStack PPC64.PPC64Registers where
  isStack PPC64.StackGP    = True
  isStack PPC64.StackFloat = True
  isStack PPC64.StackVec   = True
  isStack _                = False
  incrementsStack = const True

instance IsStack PPC32.PPC32Registers where
  isStack PPC32.StackGP    = True
  isStack PPC32.StackFloat = True
  isStack PPC32.StackVec   = True
  isStack _                = False
  incrementsStack = isStack

-- This class is for dealing with architecture-specific fixed offsets.  All
-- units are in bytes.
class ArchParamBaseOffset arch where
  paramBaseOffset :: Proxy arch -> Natural
  stackAlignment :: Proxy arch -> Natural

instance ArchParamBaseOffset PPC32 where
  paramBaseOffset _ = 8
  stackAlignment _ = 4

instance ArchParamBaseOffset PPC64 where
  paramBaseOffset _ = 48
  stackAlignment _ = 8

instance ArchParamBaseOffset X86_64 where
  paramBaseOffset _ = 8
  stackAlignment _ = 8

--------------------------------------------------------------------------------
-- FP registers sometimes need special handling.

class IsFPReg reg where
  isFPReg :: reg -> Bool

instance IsFPReg X64.X86_64Registers where
  isFPReg X64.XMM0 = True
  isFPReg X64.XMM1 = True
  isFPReg X64.XMM2 = True
  isFPReg X64.XMM3 = True
  isFPReg X64.XMM4 = True
  isFPReg X64.XMM5 = True
  isFPReg X64.XMM6 = True
  isFPReg X64.XMM7 = True
  isFPReg _ = False

instance IsFPReg PPC64.PPC64Registers where
  isFPReg PPC64.F1 = True
  isFPReg PPC64.F2 = True
  isFPReg PPC64.F3 = True
  isFPReg PPC64.F4 = True
  isFPReg PPC64.F5 = True
  isFPReg PPC64.F6 = True
  isFPReg PPC64.F7 = True
  isFPReg PPC64.F8 = True
  isFPReg PPC64.F9 = True
  isFPReg PPC64.F10 = True
  isFPReg PPC64.F11 = True
  isFPReg PPC64.F12 = True
  isFPReg PPC64.F13 = True
  isFPReg _ = False

instance IsFPReg PPC32.PPC32Registers where
  isFPReg PPC32.F1 = True
  isFPReg PPC32.F2 = True
  isFPReg PPC32.F3 = True
  isFPReg PPC32.F4 = True
  isFPReg PPC32.F5 = True
  isFPReg PPC32.F6 = True
  isFPReg PPC32.F7 = True
  isFPReg PPC32.F8 = True
  isFPReg PPC32.F9 = True
  isFPReg PPC32.F10 = True
  isFPReg PPC32.F11 = True
  isFPReg PPC32.F12 = True
  isFPReg PPC32.F13 = True
  isFPReg _ = False

class ReturnABI arch abi where
  classReturn
    :: Proxy (arch, abi)
    -> InSymbol arch abi
    -> OutSymbol arch abi

instance ReturnABI X86_64 SystemV where
  classReturn _ = \case
    SV.INTEGER -> X64.RAX
    SV.SSE -> X64.XMM0
    c -> error $ "unsuported x86_64 class:" ++ show c

instance ReturnABI PPC64 SystemV where
  classReturn _ = \case
    SV.PPC64GP -> PPC64.R3
    SV.PPC64FLOAT -> PPC64.F1

instance ReturnABI PPC32 SystemV where
  classReturn _ = \case
    SV.PPC32GP -> PPC32.R3
    SV.PPC32FLOAT -> PPC32.F1

computeReturn
  :: forall arch width abi
   . (ReturnABI arch abi, CTypeInput arch abi)
  => Proxy (arch, abi)
  -> CType
  -> OutSymbol arch abi
computeReturn proxy = classReturn proxy . ctypeInputClass proxy

--------------------------------------------------------------------------------
-- Some stuff for parsing
type Parser = MP.Parsec T.Text T.Text
