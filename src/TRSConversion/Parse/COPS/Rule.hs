{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Parse.COPS.Rule
Description : Rule parsers

This module defines parsers to parse a single rule and a block of rules in different rewriting formats.
-}
module TRSConversion.Parse.COPS.Rule (
  -- * COPS
  parseCopsRule,
  parseCopsTrsRules,
  parseCopsMsTrsRules,
)
where

import TRSConversion.Parse.COPS.Term (parseTermFuns, parseTermVars)
import TRSConversion.Parse.COPS.Utils (Parser, symbol)
import TRSConversion.Problem.Common.Rule (Rule (..), inferSigFromRules)
import TRSConversion.Problem.MsTrs.MsSig (MsSig (..))
import TRSConversion.Problem.Trs.TrsSig (Sig, TrsSig (..))
import Text.Megaparsec (many)

-- | Type synonym for a list of variables
type Vars = [String]

{- | Parse a single rule of the form @lhs->rhs@ by calling 'parseTerm' on each side of the separator.

Takes a list of variables to know whether to interpret symbols without parentheses as variables or constants.

Ignores whitespace around the @"->"@ and does not necessarily consume all input.

>>> parseTest (parseCopsRule ["x"]) "f(x) -> x"
Rule {lhs = Fun "f" [Var "x"], rhs = Var "x"}
-}
parseCopsRule :: Vars -> Parser (Rule String String)
parseCopsRule vs = do
  l <- parseTermVars vs
  _ <- symbol "->"
  r <- parseTermVars vs
  return $ Rule{lhs = l, rhs = r}

{- | Parser to extract the rules from a @RULES@ block of the [COPS TRS](http://project-coco.uibk.ac.at/problems/trs.php) format.
Takes a 'TrsSig' and calls 'parseCopsRule' 0 or more times on the input until no more rules can be parsed.
Does not necessarily consume all input.

* If given @'Vars' vs@ as a signatue, then parses rules with this variable set

* If given @'FullSig' vs fs@ as a signature, then parses rules with variable set @vs@,
infers a function signature from the rules using 'inferSigFromRules' and then asserts that this
inferred function signature is a subset of @fs@.

* @'FunSig' fs@ (specifying only function symbols) is not supported for the COPS TRS format.
-}
parseCopsTrsRules :: TrsSig String String -> Parser [Rule String String]
parseCopsTrsRules trsSig = case trsSig of
  Vars vs -> do
    rules <- many (parseCopsRule vs)
    case inferSigFromRules rules of
      Left err -> fail err
      Right _ -> return rules
  FullSig vs fs -> do
    rules <- many (parseCopsRule vs)
    case inferSigFromRules rules of
      Left err -> fail err
      Right inferredSig -> checkSignatureSubset inferredSig fs rules
  FunSig _ -> fail "The COPS format does not allow only specifying a function signature"
 where
  -- 'subList' returns whether every element of the first list is contained in the second list
  subList :: Eq a => [a] -> [a] -> Bool
  subList xs ys = all (`elem` ys) xs
  -- 'checkSignatureSubset' asserts that maybeSig is contained in inferredSig
  checkSignatureSubset :: [Sig String] -> [Sig String] -> [Rule String String] -> Parser [Rule String String]
  checkSignatureSubset inferredSig funSig rs =
    if inferredSig `subList` funSig
      then return rs
      else fail $ "Inferred signature " ++ show inferredSig ++ " not contained in input signature " ++ show funSig

{- | Parser to extract the rules from a @RULES@ block of the [COPS MSTRS](http://project-coco.uibk.ac.at/problems/mstrs.php) format.
Extracts the function symbols from the given signature and parses 0 or more rules until no more rules
can be parsed. Uses 'parseTermF' to parse each side of the rule.

Can not reuse the same logic as for (unsorted) COPS TRSs as in that case variables are specified,
whereas for MSTRSs function symbols are specified.

Does not necessarily consume all input and does not type check function applications.
See the tests for examples.
-}
parseCopsMsTrsRules :: [MsSig String String] -> Parser [Rule String String]
parseCopsMsTrsRules msSigs = do many parseMsTrsCopsRule
 where
  fsyms = map (\(MsSig fsym _) -> fsym) msSigs
  parseMsTrsCopsRule :: Parser (Rule String String)
  parseMsTrsCopsRule = do
    l <- parseTermFuns fsyms
    _ <- symbol "->"
    r <- parseTermFuns fsyms
    return $ Rule{lhs = l, rhs = r}
