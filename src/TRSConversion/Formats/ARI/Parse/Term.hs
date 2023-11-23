{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Parse.Problem.Term
Description : Term, function symbol, and variable parser

This module defines functions to parse terms from a @String@ input.
Also specifies the allowed tokens for function symbols and variables.
-}
module TRSConversion.Formats.ARI.Parse.Term (
    -- * ARI Terms
    parsePrefixTerm,

    -- * Functions and Variables
)
where

-- parseFunSymbol,
-- parseVariable,

import Data.Text (unpack)
import Text.Megaparsec (choice, count, (<?>), (<|>))

import TRSConversion.Formats.ARI.Parse.Utils (
    ARIParser,
    FunSymb,
    VarSymb,
    keyword,
    keywordToken,
    restrictedIdent,
    sExpr',
 )
import TRSConversion.Parse.Utils (Token (tokenText))
import TRSConversion.Problem.Common.Term (Term (..))
import TRSConversion.Problem.Trs.Sig (Sig (..))

{- | Parses a term in prefix notation (see also [S-expressions](https://en.wikipedia.org/wiki/S-expression)).

Tries to parse the expression as a function application first, then as a constant, and finally as a variable.
This order of priority is important as 'parsePrefixTerm' does not consume the entire input.
For example, @f x@ should be parsed as @Fun "f" [Var "x"]@, rather than as a constant @f@ with trailing input @x@.

Consumes trailing white space only after the term has been recursively parsed as far as possible.

>>> parseTest (parseTerm [Sig "f" 2, Sig "g" 1]) "f x (g c)"
Fun "f" [Var "x", Fun "g" [Fun "x" []]]
-}
parsePrefixTerm :: [Sig FunSymb] -> ARIParser (Term FunSymb VarSymb)
parsePrefixTerm funSig = parseT
  where
    parseT =
        choice (map mkParser funSig)
            <|> (Var <$> restrictedIdent <?> "variable")
    -- <|> parens parseT -- how about redundant parenthesis?
    mkParser (Sig fSymb arity)
        | arity <= 0 = Fun <$> constant <*> pure []
        | otherwise = Fun fSymb <$> sExpr' funSymb args
      where
        constant = fmap unpack <$> keywordToken (tokenText fSymb) <?> "constant"
        funSymb = keyword (tokenText fSymb) <?> "non-constant function symbol"
        args = count arity parseT
