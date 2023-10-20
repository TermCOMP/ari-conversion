{-# LANGUAGE OverloadedStrings #-}

module TRSConversion.Unparse.Infeasibility (
    unparseAriInfeasibility,
    unparseCopsInfeasibility,
) where

import Data.Containers.ListUtils (nubOrd)
import Prettyprinter (Doc, Pretty, concatWith, hsep, parens, pretty, vsep, (<+>))

import TRSConversion.Problem.CTrs.CTrs (CTrs (..), Condition (..), conditionType, orientedCTrsToTrs, rules, signature, varsCondition)
import TRSConversion.Problem.CTrs.Infeasibility (Infeasibility (..))
import qualified TRSConversion.Problem.CTrs.Infeasibility as Inf
import TRSConversion.Unparse.CTrs (prettyAriConditionType, unparseAriCSystems, unparseAriCTrsSig, unparseAriCondition, unparseCondition, unparseCopsCTrs)
import TRSConversion.Unparse.UnparseTrs (unparseCopsTrs)
import TRSConversion.Unparse.Utils (filterEmptyDocs, prettyBlock)

unparseCopsInfeasibility :: (Ord v, Pretty f, Pretty v, Ord f) => String -> Infeasibility f v -> Either String (Doc ann)
unparseCopsInfeasibility comment inf = do
    prettySystems <-
      case orientedCTrsToTrs (Inf.ctrs inf) of
        Nothing -> unparseCopsCTrs (Inf.ctrs inf)
        Just trs -> unparseCopsTrs trs
    pure $
        vsep
            [ parens $ "PROBLEM" <+> "INFEASIBILITY"
            , parens $ "COMMENT" <+> pretty comment
            , prettySystems
            , prettyBlock "VAR" $ hsep $ map pretty $ nubOrd $ concatMap varsCondition (query inf)
            , parens $ "CONDITION" <+> concatWith (\l r -> l <> "," <+> r) [unparseCondition c | c <- query inf]
            ]

unparseAriInfeasibility :: (Pretty f, Pretty v) => Infeasibility f v -> Either String (Doc ann)
unparseAriInfeasibility infProb = do
    let ctrs = Inf.ctrs infProb
    ariSig <- unparseAriCTrsSig (signature ctrs)
    let trsElements =
            [ prettyAriInfFormat infProb
            , ariSig
            , unparseAriCSystems (rules ctrs)
            , unparseAriQuery (Inf.query infProb)
            ]
    return $ vsep (filterEmptyDocs trsElements)

unparseAriQuery :: (Pretty f, Pretty v) => [Condition f v] -> Doc ann
unparseAriQuery conds = parens $ "infeasible?" <+> hsep (map unparseAriCondition conds)

prettyAriInfFormat :: Infeasibility f v -> Doc ann
prettyAriInfFormat Infeasibility { isTrs = isTrs, ctrs = CTrs {conditionType = condType}}
  --  | condType == Oriented && all (all (null . conditions)) rs =
  | isTrs = parens $ "format TRS" <+> ":problem" <+> "infeasibility"
  | otherwise = parens $ "format CTRS" <+> prettyAriConditionType condType <+> ":problem" <+> "infeasibility"
