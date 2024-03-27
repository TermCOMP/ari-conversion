{-# LANGUAGE NamedFieldPuns #-}
-- |
-- Module      : TRSConversion.Parse.Problem.Sig
-- Description : TRS signature parsers
--
-- This module defines functions to parse a 'Sig' from a @String@ input.
module TRSConversion.Formats.COPS.Parse.Sig
  ( parseCopsSig,
    parseFsymArity,
  )
where

import TRSConversion.Problem.Trs.Sig (Sig (..), Theory (..))
import Text.Megaparsec (many, takeWhile1P, (<?>))
import Data.Char (isDigit)
import TRSConversion.Formats.COPS.Parse.Utils (COPSParser, parens, lexeme, ident)
import Data.Text (unpack)

-- | Parser to extract the signature from a @SIG@ block of the COPS [extended TRS format](http://project-coco.uibk.ac.at/problems/trs.php#extended).
-- Expects a sequence of blocks @(fsym int)@ where the @int@ is the arity of the given symbol (see example below).
-- Leading and trailing spaces are ignored.
--
-- >>> parseTest parseCopsSig "(f 2) (a 0) (h 1)"
-- [Sig "f" 2,Sig "a" 0,Sig "h" 1]
parseCopsSig :: COPSParser [Sig String]
parseCopsSig = many (parens parseFsymArity)

-- | Parser to extract the function symbol and arity from a string @fsym int@ where int is
-- the arity of the given symbol (see example below). Leading and trailing spaces are removed.
--
-- Used for parsing TRSs in ARI format and the @SIG@ block of the COPS [extended TRS format](http://project-coco.uibk.ac.at/problems/trs.php#extended).
--
-- >>> parseTest parseCopsSig "fun 2"
-- Right (Sig "fun" 2)
parseFsymArity :: COPSParser (Sig String)
parseFsymArity = do
  fsym <- ident
  arity <- naturalNumber
  return Sig {fsym, arity, theory=None}

naturalNumber :: COPSParser Int
naturalNumber =
  lexeme (read . unpack <$> takeWhile1P (Just "digit") isDigit) <?> "natural number"
