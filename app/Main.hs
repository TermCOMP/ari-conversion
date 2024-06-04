{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Char (toUpper)
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import Data.Version (showVersion)
import Paths_trs_conversion (version)
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderLazy)
import System.Console.GetOpt (
  ArgDescr (NoArg, ReqArg),
  ArgOrder (Permute),
  OptDescr (..),
  getOpt,
  usageInfo,
 )
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (
  Handle,
  IOMode (WriteMode),
  hClose,
  hPutStrLn,
  openFile,
  stderr,
  stdout,
 )
import Text.XML (def, renderText)

import qualified TRSConversion.Problem.Common.MetaInfo as MetaInfo
import qualified TRSConversion.Problem.Problem as Problem
import qualified TRSConversion.Formats.ARI.Parse.Problem as ARI
import qualified TRSConversion.Formats.ARI.Parse.Utils as ARI
import TRSConversion.Formats.ARI.Unparse.Problem (unparseAriProblem)
import qualified TRSConversion.Formats.COPS.Parse.Problem as COPS
import qualified TRSConversion.Formats.COPS.Parse.Utils as COPS
import TRSConversion.Formats.COPS.Unparse.Problem (unparseCopsCOMProblem, unparseCopsProblem)
import TRSConversion.Formats.CPF3.Unparse.Problem (problemToXML)
import qualified TRSConversion.Formats.XTC.Parse.Problem as XTC
import qualified TRSConversion.Formats.XTC.Unparse.Problem as XTC
import TRSConversion.Parse.Utils (parseIO)

data Format
  = COPS
  | ARI
  | CPF3
  | XTC
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | @Config@ holds the information parsed from the options given on the command line.
data Config = Config
  { confTarget :: Maybe String
  , confSource :: Maybe String
  , confCommutationFlag :: Bool
  , confOutputFile :: Maybe FilePath
  , confAddCopsNum :: Maybe String
  , confStrategy :: Maybe String
  , confMode :: Maybe String
  }

defaultConfig :: Config
defaultConfig =
  Config
    { confSource = Nothing
    , confTarget = Nothing
    , confCommutationFlag = False
    , confOutputFile = Nothing
    , confAddCopsNum = Nothing
    , confStrategy = Nothing
    , confMode = Nothing
    }

options :: [OptDescr (Config -> IO Config)]
options = generalOptions ++ metaInfoOptions

generalOptions :: [OptDescr (Config -> IO Config)]
generalOptions =
  [ Option
      ['f']
      ["from"]
      (ReqArg (\s c -> pure c{confSource = Just s}) "FORMAT")
      "source format"
  , Option
      ['t']
      ["to"]
      (ReqArg (\s c -> pure c{confTarget = Just s}) "FORMAT")
      "target format"
  , Option
      ['o']
      ["output"]
      ( ReqArg
          ( \s c -> pure $ c{confOutputFile = Just s}
          )
          "FILE"
      )
      "write output to FILE"
  , Option
      []
      ["commutation"]
      ( NoArg (\c -> pure $ c{confCommutationFlag = True})
      )
      "print problem as a COMMUTATION\nproblem in the COPS format"
  , Option
      ['s']
      ["strategy"]
      ( ReqArg
        ( \s c -> pure $ c{confStrategy = Just s})
        "STRATEGY"
      )
      "strategy to be added to the output, if applicable"
  , Option
      ['m']
      ["mode"]
      ( ReqArg
        ( \s c -> pure $ c{confMode = Just s})
        "MODE"
      )
      "the mode -- termCOMP or CoCo"
  , Option
      ['h']
      ["help"]
      ( NoArg
          ( \_ -> usage stdout >> exitSuccess
          )
      )
      "print this message"
  , Option
      []
      ["version"]
      ( NoArg
          ( \_ -> do
              execName <- getProgName
              putStrLn (execName ++ " " ++ showVersion version)
              exitSuccess
          )
      )
      "print version"
  ]

metaInfoOptions :: [OptDescr (Config -> IO Config)]
metaInfoOptions =
  [ Option
      []
      ["cops-num"]
      (ReqArg (\s c -> pure c{confAddCopsNum = Just s}) "NUM")
      "add '@cops NUM' meta-info"
  ]

usage :: Handle -> IO ()
usage handle = do
  execName <- getProgName
  hPutStrLn handle $ description execName
  hPutStrLn handle (usageInfo "OPTIONS" generalOptions)
  hPutStrLn handle (usageInfo "META-INFO OPTIONS" metaInfoOptions)
 where
  description execName =
    unlines
      [ "Usage: " <> execName <> " -f FORMAT -t FORMAT [OPTIONS] FILE"
      , mempty
      , "Convert problems involving term-rewrite systems between formats."
      , mempty
      , "It is mandatory to give a source format (-f) and target format (-t)."
      , "Moreover the input FILE must contain a problem in the source format."
      , "The following FORMATs are supported: COPS, ARI, CPF3 (only as target)."
      , "The problem type is inferred from the input and may be:"
      , "  TRS, MSTRS, CTRS, CSTRS, CSCTRS."
      ]

{- | @trs-conversion-exe@ entry point. Can be run by calling
> stack build
> stack exec trs-conversion-exe

Currently just contains some simple hard-coded examples to illustrate
parsing and unparsing functionality.
-}
main :: IO ()
main = do
  args <- getArgs
  let (opts, nonOpts, errs) = getOpt Permute options args
  unless (null errs) $ do
    hPutStrLn stderr $ "Error(s) parsing arguments:\n" ++ concat errs
    usage stderr
    exitFailure
  conf <- foldl (>>=) (pure defaultConfig) opts
  case nonOpts of
    [inputFile] ->
      case contextFromConfig conf of
        Left err -> do
          hPutStrLn stderr err
          usage stderr
          exitFailure
        Right ctxt -> runApp ctxt inputFile
    _ -> do
      hPutStrLn stderr "Error: expected exactly one input file"
      usage stderr
      exitFailure

-- | The @Context@ holds the the configuration after validation.
data Context = Context
  { target :: Format
  , source :: Format
  , commutationFlag :: Bool
  , outputFile :: Maybe FilePath
  , addCopsNum :: Maybe String
  , strategy :: Maybe MetaInfo.Strategy
  , mode :: MetaInfo.Mode
  }

contextFromConfig :: Config -> Either String Context
contextFromConfig conf = do
  srcName <- maybe (Left "Error: missing source format (-f)") Right $ confSource conf
  trgName <- maybe (Left "Error: missing target format (-t)") Right $ confTarget conf
  src <- parseFormat srcName
  trg <- parseFormat trgName
  m <- case confMode conf of
    Nothing -> Right MetaInfo.defaultMode
    Just s -> MetaInfo.parseMode s
  s <- case confStrategy conf of
    Nothing -> Right Nothing
    Just s -> MetaInfo.parseStrategy s >>= (return . Just)
  let outFile = confOutputFile conf
  pure
    Context
      { target = trg
      , source = src
      , outputFile = outFile
      , commutationFlag = confCommutationFlag conf
      , addCopsNum = confAddCopsNum conf
      , strategy = s
      , mode = m
      }
 where
  parseFormat s = case toUpper <$> s of
    "COPS" -> Right COPS
    "ARI" -> Right ARI
    "CPF3" -> Right CPF3
    "XTC" -> Right XTC
    _ ->
      Left
        $ unlines
          [ "ERROR: '" ++ s ++ "' is not a valid FORMAT"
          , "(Must be one of: " ++ show [minBound .. maxBound :: Format] ++ ")"
          ]


runApp :: Context -> FilePath -> IO ()
runApp config inputFile = do
  fileContents <- Text.readFile inputFile

  problem <- case source config of
    COPS -> parseIO (COPS.toParser COPS.parseProblem) inputFile fileContents
    -- \| commutationFlag config ->
    --     parseIO (COPS.toParser COPS.parseCOMProblem) inputFile fileContents
    -- \| otherwise ->
    --     parseIO (COPS.toParser COPS.parseProblem) inputFile fileContents
    ARI -> parseIO (ARI.toParser (ARI.parseProblem <* ARI.noSExpr)) inputFile fileContents
    CPF3 -> do
      hPutStrLn stderr $ "ERROR: CPF3 is currently only supported as a target (not a source)"
      exitFailure
    XTC -> do
      trs <- XTC.parse inputFile
      case trs of
        Left err -> do
          hPutStrLn stderr $ "ERROR: " ++ err
          exitFailure
        Right x -> return x

  -- modify meta-info
  let metaInfo = Problem.metaInfo problem
  let copsNum = MetaInfo.copsNum metaInfo
  let problem' = problem {
        Problem.metaInfo = metaInfo{
          MetaInfo.copsNum = addCopsNum config <|> copsNum,
          MetaInfo.strategy = strategy config}
        }

  doc <- case target config of
    COPS
      | commutationFlag config -> renderPretty <$> unparseIO unparseCopsCOMProblem problem'
      | otherwise -> renderPretty <$> unparseIO unparseCopsProblem problem'
    ARI -> renderPretty <$> unparseIO unparseAriProblem problem'
    CPF3 -> pure $ renderText def (problemToXML problem')
    XTC -> renderPretty <$> unparseIO XTC.unparse problem'
  outputHandle <- case outputFile config of
    Nothing -> pure stdout
    Just fp -> openFile fp WriteMode

  LazyText.hPutStrLn outputHandle doc
  hClose outputHandle

renderPretty :: Doc ann -> LazyText.Text
renderPretty = renderLazy . layoutPretty defaultLayoutOptions

-- | Takes an unparsing function @up@ as an argument and wraps the result in the IO monad
unparseIO :: (a -> Either String (Doc ann)) -> a -> IO (Doc ann)
unparseIO up input = case up input of
  Left err -> ioError $ userError err
  Right copsMsTrs -> return copsMsTrs
