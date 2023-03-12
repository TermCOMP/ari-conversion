-- |
-- Module      : Test.Parse.Mstrs.Cops
-- Description : Parsing tests for COPS MSTRSs
--
-- This module defines test cases for the function 'parseCopsMstrs'.
-- It is non-exhaustive, but intended to highlight any obvious errors.
module Test.Parse.Mstrs.Cops (parseCopsMstrsTests) where

import Data.Conversion.Parse.ParseMstrs (parseCopsMstrs)
import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo)
import Data.Conversion.Problem.Common.Rule (Rule (..))
import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Conversion.Problem.Mstrs.MsSig (MsSig (..))
import Data.Conversion.Problem.Mstrs.Mstrs (Mstrs (..))
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)

-- | Test cases for 'parseCopsMstrs' including cases which should be parseable and cases which should fail
parseCopsMstrsTests :: Test
parseCopsMstrsTests = TestLabel "parseCopsMstrsTests" $ TestList [goodCopsMstrsTests, badCopsMstrsTests]

-- | Test cases for 'parseCopsMstrs' which should succeed and match the expected output
goodCopsMstrsTests :: Test
goodCopsMstrsTests = assertParseList "parseCopsMstrs should succeed" wellFormattedTrss parseCopsMstrs
  where
    wellFormattedTrss :: [(String, Mstrs String String String)]
    wellFormattedTrss =
      [ ("(SIG )(RULES )", Mstrs {rules = [], signature = [], sorts = Nothing, metaInfo = emptyMetaInfo}),
        ( "(SIG (0 -> Nat))\n(RULES )\n(COMMENT An MSTRS with a comment)",
          Mstrs
            { rules = [],
              signature = [MsSig "0" ([], "Nat")],
              sorts = Nothing,
              metaInfo = emptyMetaInfo {comment = Just "An MSTRS with a comment"}
            }
        ),
        ( "(SIG )(RULES a ->b)(COMMENT)",
          Mstrs
            { rules = [Rule {lhs = Var "a", rhs = Var "b"}],
              signature = [],
              sorts = Nothing,
              metaInfo = emptyMetaInfo {comment = Just ""}
            }
        ),
        ( "(SIG \n\
          \(app   List List -> List)\n\
          \(cons  Nat List -> List)\n\
          \(nil   -> List)\n\
          \(s     Nat -> Nat)\n\
          \(0     -> Nat)\n\
          \)\n\
          \(RULES\n\
          \  app(nil,ys) -> ys\n\
          \  app(cons(x,xs),ys) -> cons(x,app(xs,ys))\n\
          \)",
          Mstrs
            { rules =
                [ Rule {lhs = Fun "app" [Fun "nil" [], Var "ys"], rhs = Var "ys"},
                  Rule {lhs = Fun "app" [Fun "cons" [Var "x", Var "xs"], Var "ys"], rhs = Fun "cons" [Var "x", Fun "app" [Var "xs", Var "ys"]]}
                ],
              signature =
                [ MsSig "app" (["List", "List"], "List"),
                  MsSig "cons" (["Nat", "List"], "List"),
                  MsSig "nil" ([], "List"),
                  MsSig "s" (["Nat"], "Nat"),
                  MsSig "0" ([], "Nat")
                ],
              sorts = Nothing,
              metaInfo = emptyMetaInfo
            }
        )
      ]

-- | Malformatted examples for which it is asserted that 'parseCopsMstrs' should not succeed.
-- This list is non-exhaustive, but checks for some common problems.
badCopsMstrsTests :: Test
badCopsMstrsTests = assertFailParseList "parseCopsMstrs should fail" badTrss parseCopsMstrs
  where
    badTrss :: [String]
    badTrss =
      [ "(RULES a->b)", -- No SIG block
        "(RULES f(x) -> x)(SIG (f Nat -> Nat))", -- SIG after rules
        "(COMMENT a comment)(SIG (f Nat -> Nat))(RULES f(x)->x)", -- COMMENT block first
        "(SIG (a -> Nat) (b -> Nat))(RULES a -> b)(RULES b -> a)", -- Two RULES blocks
        "(SIG (f Nat -> Nat))(RULES f(x) -> g(x))", -- Function symbol g not in SIG
        "(VAR x)(RULES f(x)->x)" -- COPS TRS format
      ]
