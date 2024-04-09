{-# LANGUAGE NamedFieldPuns #-}
module TRSConversion.Formats.XTC.Unparse.Problem (unparse) where

import qualified TRSConversion.Problem.Trs.Sig as Ari
import qualified TRSConversion.Problem.Trs.Trs as Ari
import qualified TRSConversion.Problem.Common.MetaInfo as Ari
import qualified TRSConversion.Problem.CTrs.CTrs as Ari.CTrs
import TRSConversion.Problem.CTrs.CTrs (Condition((:==)))
import qualified TRSConversion.Problem.CSTrs.CSTrs as Ari.CSTrs
import qualified TRSConversion.Problem.CSCTrs.CSCTrs as Ari.CSCTrs
import qualified TRSConversion.Problem.Problem as Ari.Problem
import qualified TPDB.Data as TPDB
import qualified TPDB.Data.Attributes as TPDB
import qualified Data.Text as Text

import qualified TPDB.XTC as XTC
import Data.IntMap ((!))
import qualified Prettyprinter (Doc, pretty)
import Data.Foldable (find)
import TRSConversion.Problem.Trs.Sig (Sig(..))

type SrcProblem = Ari.Problem.Problem String String String
type SrcSignature = Ari.TrsSig String
type SrcRule = Ari.Rule String String
type SrcCRule = Ari.CTrs.CRule String String
type SrcTerm = Ari.Term String String
type SrcFunctionSymbol = Ari.Sig String
type SrcReplacementMap = Ari.CSTrs.ReplacementMap String
type SrcTheory = Ari.Theory
type SrcMetainfo = Ari.MetaInfo

type DstProblem = TPDB.Problem TPDB.Identifier TPDB.Identifier
type DstTrs = TPDB.TRS TPDB.Identifier TPDB.Identifier
type DstFunctionSymbol = TPDB.Funcsym
type DstSignature = TPDB.Signature
type DstTerm = TPDB.Term TPDB.Identifier TPDB.Identifier
type DstRule = TPDB.Rule DstTerm
type DstTheory = TPDB.Theory
type DstMetainfo = TPDB.Metainformation

unparse :: SrcProblem -> Either String (Prettyprinter.Doc ann)
unparse src_problem = do
    dst_problem <- convertProblem src_problem
    return $ Prettyprinter.pretty $ XTC.renderText XTC.def $ XTC.document dst_problem

convertProblem :: SrcProblem -> Either String DstProblem
convertProblem p =
    let metainfo = convertMetainfo $ Ari.Problem.metaInfo p in
    case Ari.Problem.system p of
        Ari.Problem.Trs src_trs ->
            let src_rules = Ari.rules src_trs ! 1
                src_signature = Ari.signature src_trs in
            buildProblem src_signature Nothing src_rules [] metainfo
        Ari.Problem.CSTrs src_trs ->
            let src_rules = Ari.CSTrs.rules src_trs ! 1
                replacement_map = Ari.CSTrs.replacementMap src_trs
                src_signature = Ari.CSTrs.signature src_trs in
            buildProblem src_signature (Just replacement_map) src_rules [] metainfo
        Ari.Problem.CTrs src_trs ->
            let src_rules = Ari.CTrs.rules src_trs ! 1
                src_signature = Ari.CTrs.signature src_trs in
            buildProblem src_signature Nothing [] src_rules metainfo
        Ari.Problem.CSCTrs src_trs ->
            let ctrs = Ari.CSCTrs.ctrs src_trs
                replacement_map = Ari.CSCTrs.replacementMap src_trs in
            let src_rules = Ari.CTrs.rules ctrs ! 1
                src_signature = Ari.CTrs.signature ctrs in
            buildProblem src_signature (Just replacement_map) [] src_rules metainfo
        _ -> Left "XTC export is not yet supported for the given rewrite system"

buildProblem :: SrcSignature -> Maybe SrcReplacementMap -> [SrcRule] -> [SrcCRule] -> TPDB.Metainformation -> Either String DstProblem
buildProblem src_signature replacement_map src_rules src_cond_rules metainfo = do
    dst_uncond_rules <- mapM convertRule src_rules
    let dst_cond_rules = map convertCRule src_cond_rules
    let dst_rules = dst_uncond_rules ++ dst_cond_rules
    let dst_signature = convertSignature replacement_map src_signature
    trs <- buildTrs dst_signature dst_rules
    return TPDB.Problem {
        TPDB.type_ = TPDB.Termination,
        TPDB.trs = trs,
        TPDB.strategy = Nothing,
        TPDB.full_signature = dst_signature,
        TPDB.startterm = Nothing,
        TPDB.attributes = TPDB.compute_attributes dst_rules,
        TPDB.metainformation = metainfo}

buildTrs :: DstSignature -> [DstRule] -> Either String DstTrs
buildTrs (TPDB.Signature fs) rules = Right $ TPDB.RS {
    TPDB.rules = rules,
    TPDB.signature = map (\f -> symbol (Text.unpack $ TPDB.fs_name f) (TPDB.fs_arity f)) fs,
    TPDB.separate = False}
buildTrs TPDB.HigherOrderSignature _ = Left "higher order is not yet supported"

symbol :: String -> Int -> TPDB.Identifier
symbol s a = TPDB.mk a $ Text.pack s

convertCRule :: SrcCRule -> DstRule
convertCRule (Ari.CTrs.CRule {Ari.CTrs.lhs = src_lhs, Ari.CTrs.rhs = src_rhs, Ari.CTrs.conditions = src_conditions}) =
    TPDB.Rule {
        TPDB.lhs = convertTerm src_lhs,
        TPDB.rhs = convertTerm src_rhs,
        TPDB.relation = TPDB.Strict,
        TPDB.top = False,
        TPDB.original_variable = Nothing,
        TPDB.conditions = map (\(l :== r) -> (convertTerm l, convertTerm r)) src_conditions}

convertRule :: SrcRule -> Either String DstRule
convertRule (Ari.Rule lhs rhs cost) =
    if cost /= 0 && cost /= 1 then Left "XTC only supports cost 0 and 1" else
    Right TPDB.Rule {
        TPDB.lhs = convertTerm lhs,
        TPDB.rhs = convertTerm rhs,
        TPDB.relation = if cost == 1 then TPDB.Strict else TPDB.Weak,
        TPDB.top = False,
        TPDB.original_variable = Nothing,
        TPDB.conditions = []}

convertTerm :: SrcTerm -> DstTerm
convertTerm (Ari.Var x) = TPDB.Var $ symbol x 0
convertTerm (Ari.Fun name args) =
    let arity = length args in
    TPDB.Node (symbol name arity) (map convertTerm args)

convertSignature :: Maybe SrcReplacementMap -> SrcSignature -> DstSignature
convertSignature rm (Ari.FunSig src_fs) = TPDB.Signature $ map (convertFunctionSymbol rm) src_fs

convertFunctionSymbol :: Maybe SrcReplacementMap -> SrcFunctionSymbol -> DstFunctionSymbol
convertFunctionSymbol rm (Ari.Sig {fsym, arity, theory}) =  TPDB.Funcsym {
    TPDB.fs_name = Text.pack fsym,
    TPDB.fs_arity = arity,
    TPDB.fs_theory = convertTheory theory,
    TPDB.fs_replacementmap = getReplacementMap rm fsym}

convertTheory :: SrcTheory -> Maybe DstTheory
convertTheory t = case t of
    Ari.A -> Just TPDB.A
    Ari.C -> Just TPDB.C
    Ari.AC -> Just TPDB.AC
    Ari.None -> Nothing

getReplacementMap :: Maybe SrcReplacementMap -> String -> Maybe (TPDB.Replacementmap)
getReplacementMap replacement_map name = do
    rm <- replacement_map
    (_,positions) <- find (\(n,_) -> n == name) rm
    return $ TPDB.Replacementmap positions

convertMetainfo :: SrcMetainfo -> DstMetainfo
convertMetainfo m =
    TPDB.Metainformation {
        TPDB.originalfilename = Ari.origTpdbFilename m,
        TPDB.xtcfilename = Nothing
    }
