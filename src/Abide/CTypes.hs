{-# LANGUAGE LambdaCase #-}

module Abide.CTypes
  ( CType(..)
  , ctypeByteSize
  ) where

import Numeric.Natural

data CType
  = CInt8
  | CInt16
  | CInt32
  | CInt64
  | CFloat
  | CDouble
  deriving (Eq, Ord, Show)

ctypeByteSize :: CType -> Natural
ctypeByteSize = \case
  CInt8   -> 1
  CInt16  -> 2
  CInt32  -> 4
  CInt64  -> 8
  CFloat  -> 4
  CDouble -> 8

