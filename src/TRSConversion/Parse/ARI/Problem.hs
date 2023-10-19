{- |
Module      : Data.Conversion.Parse.ARI.Problem
Description : Parse a Problem in COPS format
-}
module TRSConversion.Parse.ARI.Problem (
  parseProblem,
  parseProblem',
)
where

import TRSConversion.Parse.ARI.CSCTrs (parseAriCSCTrs')
import TRSConversion.Parse.ARI.CSTrs (parseAriCSTrs')
import TRSConversion.Parse.ARI.CTrs (parseAriCTrs')
import TRSConversion.Parse.ARI.MSTrs (parseAriMsTrs')
import TRSConversion.Parse.ARI.MetaInfo (parseAriMetaInfo)
import TRSConversion.Parse.ARI.Trs (parseAriTrs')
import TRSConversion.Parse.ARI.Utils (ARIParser, spaces, FunSymb, VarSymb, SortSymb)
import TRSConversion.Problem.Problem (Problem (Problem), FormatType (..), mapSystem)
import qualified TRSConversion.Problem.Problem as Prob
import TRSConversion.Parse.ARI.FormatType (parseFormatType)
import qualified Text.Megaparsec.Error.Builder as E
import Text.Megaparsec (MonadParsec(parseError))
import TRSConversion.Parse.ARI.Infeasibility (parseAriTRSInfeasibility', parseAriCTRSInfeasibility')
import TRSConversion.Parse.Utils (unToken)

parseProblem :: ARIParser (Problem String String String)
parseProblem = do
  prob@(Problem {Prob.system = system}) <- parseProblem'
  pure $ prob { Prob.system = mapSystem unToken unToken unToken system }

parseProblem' :: ARIParser (Problem FunSymb VarSymb SortSymb)
parseProblem' = do
  metaInfo <- parseAriMetaInfo
  spaces
  (o, ft) <- parseFormatType
  system <- case ft of
    TrsFormat n -> Prob.Trs <$> parseAriTrs' n
    MSTrsFormat n -> Prob.MSTrs <$> parseAriMsTrs' n
    CTrsFormat condType n -> Prob.CTrs <$> parseAriCTrs' condType n
    CSTrsFormat n -> Prob.CSTrs <$> parseAriCSTrs' n
    CSCTrsFormat condType n -> Prob.CSCTrs <$> parseAriCSCTrs' condType n
    InfeasibilityCTrsFormat condType -> Prob.Infeasibility <$> parseAriCTRSInfeasibility' condType
    InfeasibilityTrsFormat -> Prob.Infeasibility <$> parseAriTRSInfeasibility'
    LCTrsFormat _ -> parseError $ E.err o $ E.ulabel "LCTRS (not supported)"
  -- system <-
  --   choice
  --     [ try $ Prob.Trs <$> parseAriTrs
  --     , try $ Prob.MSTrs <$> parseAriMsTrs
  --     , try $ Prob.CTrs <$> parseAriCTrs
  --     , try $ Prob.CSTrs <$> parseAriCSTrs
  --     , Prob.CSCTrs <$> parseAriCSCTrs
  --     ]
  pure $
    Problem
      { Prob.metaInfo = metaInfo
      , Prob.system = system
      }
