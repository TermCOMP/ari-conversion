module TRSConversion.Formats.XTC.Parse.Problem (parse) where

import qualified TRSConversion.Problem.Trs.Sig as Ari
import qualified TRSConversion.Problem.Trs.Trs as Ari
import qualified TRSConversion.Problem.CTrs.CTrs as Ari.CTrs
import TRSConversion.Problem.CTrs.CTrs (Condition((:==)))
import qualified TRSConversion.Problem.Problem as Ari.Problem
import qualified TRSConversion.Problem.CSTrs.CSTrs as Ari.CSTrs
import qualified TRSConversion.Problem.CSCTrs.CSCTrs as Ari.CSCTrs
import qualified TPDB.Data as TPDB
import qualified TPDB.XTC.Read as Read
import qualified TRSConversion.Problem.Common.MetaInfo as MetaInfo ( emptyMetaInfo )
import qualified Data.IntMap as IntMap
import qualified Data.Text as Text
import TPDB.Data (Funcsym)
import Data.Maybe ( isNothing )

type SrcProblem = TPDB.Problem TPDB.Identifier TPDB.Identifier
type SrcFunctionSymbol = Funcsym
type SrcSignature = TPDB.Signature
type SrcRule = (SrcTerm, SrcTerm)
type SrcCRule = (SrcTerm, SrcTerm, [SrcRule])
type SrcTerm = TPDB.Term TPDB.Identifier TPDB.Identifier
type SrcTheory = TPDB.Theory

type DstProblem = Ari.Problem.Problem String String String
type DstSignature = Ari.TrsSig String
type DstRule = Ari.Rule String String
type DstCRule = Ari.CTrs.CRule String String
type DstTerm = Ari.Term String String
type DstFunctionSymbol = Ari.Sig String
type DstReplacementMap = Ari.CSTrs.ReplacementMap String
type DstTheory = Ari.Theory

parse :: FilePath -> IO (Either String DstProblem)
parse f = do
    src_problem <- Read.readProblemF f
    return $ convertProblem src_problem

buildCTrs :: [DstCRule] -> DstSignature -> Ari.CTrs.CTrs String String
buildCTrs rules sig = Ari.CTrs.CTrs {
    Ari.CTrs.rules = IntMap.singleton 1 rules,
    Ari.CTrs.signature = sig,
    Ari.CTrs.numSystems = 1,
    Ari.CTrs.conditionType = Ari.CTrs.Oriented}

convertProblem :: SrcProblem -> Either String DstProblem
convertProblem p = do
    let src_trs = TPDB.trs p
    let src_signature = TPDB.full_signature p
    let src_cond_rules = TPDB.cond_rules src_trs
    let (dst_cond_rules, dst_rules) = if null src_cond_rules then ([], map convertRule (TPDB.strict_rules src_trs) ++ map convertRelativeRule (TPDB.weak_rules src_trs))
        else (map convertConditionalRule $ TPDB.cond_rules src_trs ++ map (\(l,r) -> (l,r,[])) (TPDB.strict_rules src_trs), [])
    dst_signature <- convertSignature src_signature
    replacement_map <- convertReplacementMap src_signature
    let system = case replacement_map of
            Nothing ->
                if null dst_cond_rules then
                    Ari.Problem.Trs $ Ari.Trs {
                        Ari.rules = IntMap.singleton 1 dst_rules,
                        Ari.signature = dst_signature,
                        Ari.numSystems = 1}
                else Ari.Problem.CTrs $ buildCTrs dst_cond_rules dst_signature
            Just rm ->
                if null dst_cond_rules then
                    Ari.Problem.CSTrs $ Ari.CSTrs.CSTrs {
                        Ari.CSTrs.rules = IntMap.singleton 1 dst_rules,
                        Ari.CSTrs.signature = dst_signature,
                        Ari.CSTrs.replacementMap = rm,
                        Ari.CSTrs.numSystems = 1}
                else
                    Ari.Problem.CSCTrs $ Ari.CSCTrs.CSCTrs {
                        Ari.CSCTrs.replacementMap = rm,
                        Ari.CSCTrs.ctrs = buildCTrs dst_cond_rules dst_signature}
    return $ Ari.Problem.Problem {
        Ari.Problem.system = system,
        Ari.Problem.metaInfo = MetaInfo.emptyMetaInfo}

convertRule :: SrcRule -> DstRule
convertRule (lhs, rhs) = Ari.mkRule (convertTerm lhs) (convertTerm rhs)

convertRelativeRule :: SrcRule -> DstRule
convertRelativeRule (lhs, rhs) = Ari.mkRuleWithCost (convertTerm lhs) (convertTerm rhs) 0

convertConditionalRule :: SrcCRule -> DstCRule
convertConditionalRule (src_lhs, src_rhs, src_conditions) =
    let dst_conditions = map (\(l,r) -> convertTerm l :== convertTerm r) src_conditions
        dst_lhs = convertTerm src_lhs
        dst_rhs = convertTerm src_rhs in
    Ari.CTrs.CRule {
        Ari.CTrs.lhs = dst_lhs,
        Ari.CTrs.rhs = dst_rhs,
        Ari.CTrs.conditions = dst_conditions
    }

convertTerm :: SrcTerm -> DstTerm
convertTerm (TPDB.Var x) = Ari.Var $ Text.unpack (TPDB.name x)
convertTerm (TPDB.Node f args) = Ari.Fun (Text.unpack $ TPDB.name f) (map convertTerm args)

convertSignature :: SrcSignature -> Either String DstSignature
convertSignature (TPDB.Signature fs) = Right $ Ari.FunSig $ map convertFunctionSymbol fs
convertSignature TPDB.HigherOrderSignature = Left "higher order is not yet supported"

convertFunctionSymbol :: SrcFunctionSymbol -> DstFunctionSymbol
convertFunctionSymbol f = Ari.Sig {
    Ari.fsym = Text.unpack $ TPDB.fs_name f,
    Ari.arity = TPDB.fs_arity f,
    Ari.theory = convertTheory $ TPDB.fs_theory f
}

convertTheory :: Maybe SrcTheory -> DstTheory
convertTheory Nothing = Ari.None
convertTheory (Just t) = case t of
    TPDB.A -> Ari.A
    TPDB.C -> Ari.C
    TPDB.AC -> Ari.AC

convertReplacementMap :: SrcSignature -> Either String (Maybe DstReplacementMap)
convertReplacementMap (TPDB.Signature fs) = Right $
    if all (isNothing . TPDB.fs_replacementmap) fs then Nothing
    else
        Just $ map (\f ->
            let f' = Text.unpack $ TPDB.fs_name f in
            case TPDB.fs_replacementmap f of
                Just (TPDB.Replacementmap m) -> (f',m)
                Nothing -> (f',[1..TPDB.fs_arity f])) fs
convertReplacementMap TPDB.HigherOrderSignature = Left "higher order is not yet supported"
