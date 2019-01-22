{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module : Abide.Parse.ABI
Copyright : (c) 2018 Galois Inc.
License : BSD3 (see LICENSE)

All the parsers for FSTs in general.
-}
module Abide.Parse.Common where

import           Control.Lens ( (^.) )
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Tuple.Select ( sel1 )
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

import Abide.Types

parseFST :: forall arch abi i o.
            (InSymbol arch abi ~ i, OutSymbol arch abi ~ o)
         => Parser i -> Parser o -> Parser (FST arch abi)
parseFST i o = mkFST <$> MP.some (edge i o)

mkFST :: forall arch abi i o. (InSymbol arch abi ~ i, OutSymbol arch abi ~ o) => [Edge UID i o] -> FST arch abi
mkFST es = FST $ M.fromList $ map (allFrom es . (^. src)) es

allFrom :: [Edge UID i o] -> UID -> (UID, [Edge UID i o])
allFrom es s = (s, filter ((==s) . (^. src)) es)
               
edge :: Parser i -> Parser o -> Parser (Edge UID i o)
edge pi po = do
  src <- MPL.decimal
  MP.space1
  dst <- MPL.decimal
  MP.space1
  i <- pi
  MP.space1
  o <- po
  MP.eol
  return $ Edge src dst i o

