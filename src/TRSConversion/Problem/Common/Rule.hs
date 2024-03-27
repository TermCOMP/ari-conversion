{- |
Module      : TRSConversion.Problem.Common.Rule
Description : Rule type and helpers

This module contains the 'Rule' type definition to represent rewriting rules and helper functions for Rules.
-}
{-# LANGUAGE NamedFieldPuns #-}
module TRSConversion.Problem.Common.Rule (
  -- * Types
  Rule (..),
  map,
  mapRule,

  -- * Helper functions
  inferSigFromRules,
  ruleVars,
  ruleFuns,
  mkRule,
  mkRuleWithCost
)
where

import Prelude hiding (map)
import Data.List (nub, sort)
import TRSConversion.Problem.Common.Term (Term (..), termFunArities)
import qualified TRSConversion.Problem.Common.Term as Term
import TRSConversion.Problem.Trs.Sig (Sig, checkDistinctSig)
import Data.Containers.ListUtils (nubOrd)

-- | Datatype representing a rewrite rule @lhs->rhs@.
data Rule f v = Rule
  { lhs :: Term f v
  -- ^ The left-hand side of the rule
  , rhs :: Term f v
  -- ^ The right-hand side of the rule
  , cost :: Int
  -- ^ The cost of the rule. 1 for standard rewriting, 0 for relative rules.
  --   Other costs are of interest for complexity and probabilistic termination.
  }
  deriving (Eq, Show)

-- | see @map@
mapRule :: (f -> f') -> (v -> v') -> Rule f v -> Rule f' v'
mapRule = map

-- | @map f v@ maps the function @f@ over all function symbols and the
-- function @v@ over all variables in the rule
map :: (f -> f') -> (v -> v') -> Rule f v -> Rule f' v'
map f v (Rule{lhs = l, rhs = r, cost = c}) =
  mkRuleWithCost (Term.map f v l) (Term.map f v r) c

{- | Returns a list of the function symbols appearing on both sides of a 'Rule' and their arities (number of arguments).
Removes duplicates and asserts that each function symbol name has at most one arity.

>>>  ruleFunArities $ Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = Var "x"})
Right [Sig "f" 2]

>>> ruleFunArities $ Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = Fun "f" [Var "x"]}
Left "A function symbol appears multiple times in signature ...
-}
ruleFunArities :: (Eq f) => Rule f v -> Either String [Sig f]
ruleFunArities (Rule {lhs, rhs}) = do
  lhsArities <- termFunArities lhs
  rhsArities <- termFunArities rhs
  checkDistinctSig $ nub (lhsArities ++ rhsArities)

{- | Infer a signature from a list of rules by applying 'ruleFunArities' to each rule and then
checking that the union of the inferred signatures contain consistent arities.

>>> inferSigFromRules [Rule {lhs = Fun "f" [Var "x"], rhs = Var "x"}, Rule {lhs = Fun "g" [Var "x"], rhs = Var "x"}]
Right [Sig "f" 1,Sig "g" 1]

>>> inferSigFromRules [Rule {lhs = Fun "a" [Var "x"], rhs = Var "x"}, Rule {lhs = Fun "a" [], rhs = Fun "b" []}]
Left "A function symbol appears multiple times in signature...
-}
inferSigFromRules :: (Ord f) => [Rule f v] -> Either String [Sig f]
inferSigFromRules rs = do
  case mapM ruleFunArities rs of
    -- Each individual signature might be consistent but we need to check their union
    Right sigLists -> checkDistinctSig . sort $ nubOrd (concat sigLists)
    Left err -> Left err

{- | Extract a list of variables appearing in both sides of a list of rules.
Duplicates are removed with 'nub'. Not very efficient, but it works.

>>> ruleVars [Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = Var "x"}
["x", "y"]
-}
ruleVars :: Ord v => [Rule f v] -> [v]
ruleVars rs = nubOrd $ concatMap (\(Rule {lhs, rhs}) -> Term.vars lhs ++ Term.vars rhs) rs

ruleFuns :: Ord f => [Rule f v] -> [f]
ruleFuns rs = nubOrd $ concatMap (\r -> Term.funs (lhs r) ++ Term.funs (rhs r)) rs

mkRule :: Term f v -> Term f v -> Rule f v
mkRule s t = Rule s t 1

mkRuleWithCost :: Term f v -> Term f v -> Int -> Rule f v
mkRuleWithCost = Rule
