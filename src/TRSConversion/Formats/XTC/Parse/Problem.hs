module TRSConversion.Formats.XTC.Parse.Problem (parse) where

import qualified TRSConversion.Problem.Trs.Trs as Ari
import qualified TRSConversion.Problem.Problem as Ari.Problem
import qualified TPDB.Data as TPDB
import qualified TPDB.Input.File as Input
import qualified TRSConversion.Problem.Common.MetaInfo as MetaInfo ( emptyMetaInfo )
import qualified Data.IntMap as IntMap
import qualified Data.Text as Text

type SrcTrs = TPDB.TRS TPDB.Identifier TPDB.Identifier
type SrcFunctionSymbol = TPDB.Identifier
type SrcSignature = [SrcFunctionSymbol]
type SrcRule = (SrcTerm, SrcTerm)
type SrcTerm = TPDB.Term TPDB.Identifier TPDB.Identifier

type DstProblem = Ari.Problem.Problem String String String
type DstTrs = Ari.Trs String String
type DstSignature = Ari.TrsSig String
type DstRule = Ari.Rule String String
type DstTerm = Ari.Term String String
type DstFunctionSymbol = Ari.Sig String

parse :: FilePath -> IO (Either String DstProblem)
parse f = do
    src_trs <- Input.get_trs f
    let dst_trs = convertTrs src_trs
    return $ Right $ Ari.Problem.Problem {
        Ari.Problem.metaInfo = MetaInfo.emptyMetaInfo,
        Ari.Problem.system = Ari.Problem.Trs dst_trs}

convertTrs :: SrcTrs -> DstTrs
convertTrs trs =
    let rules = map convertRule (TPDB.strict_rules trs)
        signature = convertSignature $ TPDB.signature trs in
    Ari.Trs {
        Ari.rules = IntMap.singleton 0 rules,
        Ari.signature = signature,
        Ari.numSystems = 1}

convertRule :: SrcRule -> DstRule
convertRule (lhs, rhs) = Ari.Rule (convertTerm lhs) (convertTerm rhs)

convertTerm :: SrcTerm -> DstTerm
convertTerm (TPDB.Var x) = Ari.Var $ Text.unpack (TPDB.name x)
convertTerm (TPDB.Node f args) = Ari.Fun (Text.unpack $ TPDB.name f) (map convertTerm args)

convertSignature :: SrcSignature -> DstSignature
convertSignature fs = Ari.FunSig $ map convertFunctionSymbol fs

convertFunctionSymbol :: SrcFunctionSymbol -> DstFunctionSymbol
convertFunctionSymbol f = Ari.Sig (Text.unpack $ TPDB.name f) (TPDB.arity f)
