{-# LANGUAGE NamedFieldPuns #-}
-- |
-- Module      : TRSConversion.Problem.MsTrs.MsSig
-- Description : Type definition for MSTRS signature
--
-- This module contains the 'MsSig' type definition for specifying the signature of a many-sorted
-- term rewriting system and helper functions.
module TRSConversion.Problem.MsTrs.MsSig
  ( -- * Many-sorted signature datatype
    MsSig (..),
    map,
    -- * Helper functions
    inferSorts,
  )
where

import Prelude hiding (map)
import Data.List (nub)
import TRSConversion.Problem.Trs.Sig (Theory)

-- | Datatype for the signature of a single function symbol in a many-sorted TRS ('MsTrs').
--
-- For example, a function symbol @cons@ which takes a @Nat@ and @List@ and returns a @List@ can be written as
--  > MsSig "cons" (["Nat"], "List").
-- A constant @n@ of type @Nat@ can be written as
--  > MsSig "n" ([], "Nat").
data MsSig f s
  = MsSig
    { funsym :: f
      -- ^ The function symbol
    , sort :: ([s], s)
      -- ^ A list of the input sorts and the single output sorts of the function symbol
    , theory :: Theory}
  deriving (Eq, Show)

map :: (f -> f') -> (s -> s') -> MsSig f s -> MsSig f' s'
map f s (MsSig {funsym=g, sort=(args,res), theory}) = MsSig {funsym=(f g), sort=(fmap s args, s res), theory}

-- | Function to infer a list of all unique sorts in an 'MsSig'.
--
-- >>> inferSorts [MsSig "cons" (["Nat","List"] "List"), MsSig "treeList" (["Tree"] "List")]
-- ["Nat", "List", "Tree"]
inferSorts :: Eq s => [MsSig f s] -> [s]
inferSorts = nub . concatMap (\(MsSig {sort=(inSorts, outSort)}) -> outSort : inSorts)
