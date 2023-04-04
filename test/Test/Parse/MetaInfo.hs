-- |
-- Module      : Test.Parse.MetaInfo
-- Description : Parsing tests for MetaInfo
--
-- This module defines test cases for functions used to parse TRS meta-information (e.g. comment, authors, etc.).
module Test.Parse.MetaInfo (parseMetaInfoTests) where

import Data.Conversion.Parse.Problem.MetaInfo (parseAriMetaInfo, parseCopsMetaInfo)
import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo)
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)

-- | Tests for parsing rules in COPS format, including tests for which parsing should succeed and for which parsing should fail
parseMetaInfoTests :: Test
parseMetaInfoTests = TestLabel "Test.Parse.MetaInfo" $ TestList [parseAriMetaTests, badAriMetaTests, parseCopsMetaTests]

-- | Test cases for which 'parseAriMetaInfo' should succeed and produce the given output
parseAriMetaTests :: Test
parseAriMetaTests = assertParseList "parseAriMetaInfo should succeed" validMetaInfo parseAriMetaInfo
  where
    validMetaInfo :: [(String, MetaInfo)]
    validMetaInfo =
      [ ("(meta-info (origin \"COPS #20\"))", emptyMetaInfo {origin = Just "COPS #20"}),
        ("(meta-info ( doi \"10.1007/11805618_6\"  )  )", emptyMetaInfo {doi = Just "10.1007/11805618_6"}),
        ("(meta-info (submitted \"Person 1\" \"Person-2\"))", emptyMetaInfo {submitted = Just ["Person 1", "Person-2"]}),
        ("(meta-info (comment \"Coment with (parentheses))\"))", emptyMetaInfo {comment = Just "Coment with (parentheses))"}),
        ("", emptyMetaInfo),
        ( "(meta-info (origin \"COPS #20\"))\
          \ (meta-info (comment \"comment\"))\
          \ (meta-info (submitted   \"Person 1\")) \
          \ (meta-info (comment \"comment\"))",
          emptyMetaInfo
            { comment = Just "comment",
              origin = Just "COPS #20",
              submitted = Just ["Person 1"]
            }
        )
      ]

-- | Test cases for which 'parseAriMetaInfo' should fail
badAriMetaTests :: Test
badAriMetaTests = assertFailParseList "parseAriMetaInfo should fail" badMetaInfo parseAriMetaInfo
  where
    badMetaInfo :: [String]
    badMetaInfo =
      [ "(meta-info (COMMENT \"C1\"))",
        " ",
        "\n",
        "(meta-info (origin someOrigin))", -- No quotation marks around someOrigin
        "(meta-info (submitted Person 1))", -- No quotation marks
        "(meta-info (submitted \"Person 1\" Person2))",
        "(meta-info (invalidKey someValue))",
        "(meta-info (doi ))", -- No value
        "(meta-info (comment \"Coment with \"quotes\"\"))" -- Nested quotation marks (not currently supported)
      ]

-- | Test cases for which 'parseCopsMetaInfo' should succeed and produce the given output
parseCopsMetaTests :: Test
parseCopsMetaTests = assertParseList "parseCopsMetaInfo should succeed" validMetaInfo parseCopsMetaInfo
  where
    validMetaInfo :: [(String, MetaInfo)]
    validMetaInfo =
      [ ("  some example comment  ", emptyMetaInfo {comment = Just "  some example comment  "}),
        ("comment (with parentheses)", emptyMetaInfo {comment = Just "comment (with parentheses)"}),
        ("comment (with (nested parentheses) and \\ escaped \n symbols)", emptyMetaInfo {comment = Just "comment (with (nested parentheses) and \\ escaped \n symbols)"}),
        ("comment \"with quotation marks\"", emptyMetaInfo {comment = Just "comment \"with quotation marks\""}),
        ("", emptyMetaInfo {comment = Just ""}),
        (" ", emptyMetaInfo {comment = Just " "})
      ]