module TRSConversion.Formats.XTC.Parse.Problem (parse) where

import qualified TRSConversion.Problem.Trs.Trs as Ari
import qualified TRSConversion.Problem.Problem as Ari.Problem
import qualified TRSConversion.Problem.CSTrs.CSTrs as Ari.CSTrs
import qualified TPDB.Data as TPDB
import qualified TPDB.XTC.Read as Read
import qualified TRSConversion.Problem.Common.MetaInfo as MetaInfo ( emptyMetaInfo )
import qualified Data.IntMap as IntMap
import qualified Data.Text as Text
import TPDB.Data (Funcsym)
import Data.Maybe ( isNothing )
import qualified TRSConversion.Problem.Common.Rule as Ari

type SrcProblem = TPDB.Problem TPDB.Identifier TPDB.Identifier
type SrcFunctionSymbol = Funcsym
type SrcSignature = TPDB.Signature
type SrcRule = (SrcTerm, SrcTerm)
type SrcTerm = TPDB.Term TPDB.Identifier TPDB.Identifier

type DstProblem = Ari.Problem.Problem String String String
type DstSignature = Ari.TrsSig String
type DstRule = Ari.Rule String String
type DstTerm = Ari.Term String String
type DstFunctionSymbol = Ari.Sig String
type DstReplacementMap = Ari.CSTrs.ReplacementMap String

parse :: FilePath -> IO (Either String DstProblem)
parse f = do
    src_problem <- Read.readProblemF f
    return $ convertProblem src_problem

convertProblem :: SrcProblem -> Either String DstProblem
convertProblem p = do
    let src_trs = TPDB.trs p
    let src_signature = TPDB.full_signature p
    let dst_rules = map convertRule (TPDB.strict_rules src_trs) ++ map convertRelativeRule (TPDB.weak_rules src_trs)
    dst_signature <- convertSignature src_signature
    replacement_map <- convertReplacementMap src_signature
    let system = case replacement_map of
            Nothing ->
                Ari.Problem.Trs $ Ari.Trs {
                    Ari.rules = IntMap.singleton 0 dst_rules,
                    Ari.signature = dst_signature,
                    Ari.numSystems = 1}
            Just rm ->
                Ari.Problem.CSTrs $ Ari.CSTrs.CSTrs {
                    Ari.CSTrs.rules = IntMap.singleton 0 dst_rules,
                    Ari.CSTrs.signature = dst_signature,
                    Ari.CSTrs.replacementMap = rm,
                    Ari.CSTrs.numSystems = 1}
    return $ Ari.Problem.Problem {
        Ari.Problem.system = system,
        Ari.Problem.metaInfo = MetaInfo.emptyMetaInfo}

convertRule :: SrcRule -> DstRule
convertRule (lhs, rhs) = Ari.mkRule (convertTerm lhs) (convertTerm rhs)

convertRelativeRule :: SrcRule -> DstRule
convertRelativeRule (lhs, rhs) = Ari.mkRuleWithCost (convertTerm lhs) (convertTerm rhs) 0

convertTerm :: SrcTerm -> DstTerm
convertTerm (TPDB.Var x) = Ari.Var $ Text.unpack (TPDB.name x)
convertTerm (TPDB.Node f args) = Ari.Fun (Text.unpack $ TPDB.name f) (map convertTerm args)

convertSignature :: SrcSignature -> Either String DstSignature
convertSignature (TPDB.Signature fs) = Right $ Ari.FunSig $ map convertFunctionSymbol fs
convertSignature TPDB.HigherOrderSignature = Left "higher order is not yet supported"

convertFunctionSymbol :: SrcFunctionSymbol -> DstFunctionSymbol
convertFunctionSymbol f = Ari.Sig (Text.unpack $ TPDB.fs_name f) (TPDB.fs_arity f)

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
