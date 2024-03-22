module TRSConversion.Formats.XTC.Unparse.Problem (unparse) where

import qualified TRSConversion.Problem.Trs.Trs as Ari
import qualified TRSConversion.Problem.CSTrs.CSTrs as Ari.CSTrs
import qualified TRSConversion.Problem.Problem as Ari.Problem
import qualified TPDB.Data as TPDB
import qualified TPDB.Data.Attributes as TPDB
import qualified Data.Text as Text

import qualified TPDB.XTC as XTC
import Data.IntMap ((!))
import qualified Prettyprinter (Doc, pretty)
import Data.Foldable (find)

type SrcProblem = Ari.Problem.Problem String String String
type SrcSignature = Ari.TrsSig String
type SrcRule = Ari.Rule String String
type SrcTerm = Ari.Term String String
type SrcFunctionSymbol = Ari.Sig String
type SrcReplacementMap = Ari.CSTrs.ReplacementMap String

type DstProblem = TPDB.Problem TPDB.Identifier TPDB.Identifier
type DstTrs = TPDB.TRS TPDB.Identifier TPDB.Identifier
type DstFunctionSymbol = TPDB.Funcsym
type DstSignature = TPDB.Signature
type DstTerm = TPDB.Term TPDB.Identifier TPDB.Identifier
type DstRule = TPDB.Rule DstTerm

unparse :: SrcProblem -> Either String (Prettyprinter.Doc ann)
unparse src_problem = do
    dst_problem <- convertProblem src_problem
    return $ Prettyprinter.pretty $ XTC.renderText XTC.def $ XTC.document dst_problem

convertProblem :: SrcProblem -> Either String DstProblem
convertProblem p =
    case Ari.Problem.system p of
        Ari.Problem.Trs src_trs ->
            let src_rules = Ari.rules src_trs ! 1
                src_signature = Ari.signature src_trs in
            buildProblem src_signature Nothing src_rules
        Ari.Problem.CSTrs src_trs ->
            let src_rules = Ari.CSTrs.rules src_trs ! 1
                replacement_map = Ari.CSTrs.replacementMap src_trs
                src_signature = Ari.CSTrs.signature src_trs in
            buildProblem src_signature (Just replacement_map) src_rules
        _ -> Left "XTC export is not yet supported for the given rewrite system"

buildProblem :: SrcSignature -> Maybe SrcReplacementMap -> [SrcRule] -> Either String DstProblem
buildProblem src_signature replacement_map src_rules = do
    let dst_rules = map convertRule src_rules
    let dst_signature = convertSignature replacement_map src_signature
    trs <- buildTrs dst_signature dst_rules
    return TPDB.Problem {
        TPDB.type_ = TPDB.Termination,
        TPDB.trs = trs,
        TPDB.strategy = Nothing,
        TPDB.full_signature = dst_signature,
        TPDB.startterm = Nothing,
        TPDB.attributes = TPDB.compute_attributes dst_rules}

buildTrs :: DstSignature -> [DstRule] -> Either String DstTrs
buildTrs (TPDB.Signature fs) rules = Right $ TPDB.RS {
    TPDB.rules = rules,
    TPDB.signature = map (\f -> symbol (Text.unpack $ TPDB.fs_name f) (TPDB.fs_arity f)) fs,
    TPDB.separate = False}
buildTrs TPDB.HigherOrderSignature _ = Left "higher order is not yet supported"

symbol :: String -> Int -> TPDB.Identifier
symbol s a = TPDB.mk a $ Text.pack s

convertRule :: SrcRule -> DstRule
convertRule (Ari.Rule lhs rhs) = TPDB.Rule {
    TPDB.lhs = convertTerm lhs,
    TPDB.rhs = convertTerm rhs,
    TPDB.relation = TPDB.Strict,
    TPDB.top = False,
    TPDB.original_variable = Nothing}

convertTerm :: SrcTerm -> DstTerm
convertTerm (Ari.Var x) = TPDB.Var $ symbol x 0
convertTerm (Ari.Fun name args) =
    let arity = length args in
    TPDB.Node (symbol name arity) (map convertTerm args)

convertSignature :: Maybe SrcReplacementMap -> SrcSignature -> DstSignature
convertSignature rm (Ari.FunSig src_fs) = TPDB.Signature $ map (convertFunctionSymbol rm) src_fs

convertFunctionSymbol :: Maybe SrcReplacementMap -> SrcFunctionSymbol -> DstFunctionSymbol
convertFunctionSymbol rm (Ari.Sig name arity) =  TPDB.Funcsym {
    TPDB.fs_name = Text.pack name,
    TPDB.fs_arity = arity,
    TPDB.fs_theory = Nothing,
    TPDB.fs_replacementmap = getReplacementMap rm name}

getReplacementMap :: Maybe SrcReplacementMap -> String -> Maybe (TPDB.Replacementmap)
getReplacementMap replacement_map name = do
    rm <- replacement_map
    (_,positions) <- find (\(n,_) -> n == name) rm
    return $ TPDB.Replacementmap positions
