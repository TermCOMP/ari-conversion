import Test.HUnit
import Test.Parse.MetaInfo (parseMetaInfoTests)
import Test.Parse.Mstrs.Ari (parseAriMstrsTests)
import Test.Parse.Mstrs.Cops (parseCopsMstrsTests)
import Test.Parse.Mstrs.MsSig (parseMsSigTests)
import Test.Parse.Rule (parseRuleTests)
import Test.Parse.Term (parseTermTests)
import Test.Parse.Trs.Ari (parseAriTests)
import Test.Parse.Trs.Cops (parseCopsTests)
import Test.Parse.Trs.Sig (parseSigTests)
import Test.Unparse.Problem.MetaInfo (unparseMetaInfoTests)
import Test.Unparse.Problem.MsSig (unparseMsSigTests)
import Test.Unparse.Problem.Rule (unparseRuleTests)
import Test.Unparse.Problem.Term (unparseTermTests)
import Test.Unparse.Problem.TrsSig (unparseSigTests)
import Test.Unparse.UnparseMstrs (unparseMstrsTests)
import Test.Unparse.UnparseTrs (unparseTrsTests)

-- | The testing entry point. Runs each test in turn and logs output to the console.
main :: IO ()
main = do
  _ <- parsingTests
  _ <- unparsingTests
  putStrLn "Testing complete."

-- | Collect and run tests for parsing
parsingTests :: IO ()
parsingTests = do
  putStrLn "Runnning parsing tests"
  _ <-
    runTestTT $
      TestList
        [ parseTermTests,
          parseMetaInfoTests,
          parseRuleTests,
          parseSigTests,
          parseMsSigTests,
          parseCopsTests,
          parseAriTests,
          parseCopsMstrsTests,
          parseAriMstrsTests
        ]
  putStrLn "---"

-- | Collect and run tests for unparsing
unparsingTests :: IO ()
unparsingTests = do
  putStrLn "Runnning unparsing tests"
  _ <-
    runTestTT $
      TestList
        [ unparseTermTests,
          unparseSigTests,
          unparseMsSigTests,
          unparseMetaInfoTests,
          unparseRuleTests,
          unparseTrsTests,
          unparseMstrsTests
        ]
  putStrLn "---"
