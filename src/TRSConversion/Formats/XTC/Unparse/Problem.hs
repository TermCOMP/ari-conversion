module TRSConversion.Formats.XTC.Unparse.Problem (unparse) where

import qualified TRSConversion.Problem.Trs.Trs as Ari
import qualified TRSConversion.Problem.Problem as Ari.Problem
import qualified TPDB.Data as TPDB
import qualified TPDB.Data.Attributes as TPDB
import qualified Data.Text as Text

import qualified TPDB.XTC as XTC
import Data.IntMap ((!))
import qualified Prettyprinter (Doc, pretty)


type SrcProblem = Ari.Problem.Problem String String String
type SrcTrs = Ari.Trs String String
type SrcSignature = Ari.TrsSig String
type SrcRule = Ari.Rule String String
type SrcTerm = Ari.Term String String
type SrcFunctionSymbol = Ari.Sig String

type DstProblem = TPDB.Problem TPDB.Identifier TPDB.Identifier
type DstTrs = TPDB.TRS TPDB.Identifier TPDB.Identifier
type DstFunctionSymbol = TPDB.Identifier
type DstSignature = [DstFunctionSymbol]
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
            let dst_trs = convertTrs src_trs in
            let signature = TPDB.signature dst_trs
                rules = TPDB.rules dst_trs in
            let full_signature = fullSignature signature
                attributes = TPDB.compute_attributes rules in
            Right $ TPDB.Problem {
                TPDB.type_ = TPDB.Termination,
                TPDB.trs = dst_trs,
                TPDB.strategy = Nothing,
                TPDB.full_signature = full_signature,
                TPDB.startterm = Nothing,
                TPDB.attributes = attributes
            }
        _ -> Left "XTC export is not yet supported for the given rewrite system"

fullSignature :: DstSignature -> TPDB.Signature
fullSignature fs = TPDB.Signature $ map (\x -> TPDB.Funcsym {
    TPDB.fs_name = TPDB.name x,
    TPDB.fs_arity = TPDB.arity x,
    TPDB.fs_theory = Nothing,
    TPDB.fs_replacementmap = Nothing}) fs

convertTrs :: SrcTrs -> DstTrs
convertTrs trs =
    let rules = map convertRule (Ari.rules trs ! 1)
        signature = convertSignature $ Ari.signature trs in
    TPDB.RS {
        TPDB.rules = rules,
        TPDB.signature = signature,
        TPDB.separate = False}

convertRule :: SrcRule -> DstRule
convertRule (Ari.Rule lhs rhs) = TPDB.Rule {
    TPDB.lhs = convertTerm lhs,
    TPDB.rhs = convertTerm rhs,
    TPDB.relation = TPDB.Strict,
    TPDB.top = False,
    TPDB.original_variable = Nothing}

symbol :: String -> Int -> TPDB.Identifier
symbol s a = TPDB.mk a $ Text.pack s

convertTerm :: SrcTerm -> DstTerm
convertTerm (Ari.Var x) = TPDB.Var $ symbol x 0
convertTerm (Ari.Fun name args) =
    let arity = length args in
    TPDB.Node (symbol name arity) (map convertTerm args)

convertSignature :: SrcSignature -> DstSignature
convertSignature (Ari.FunSig fs) = map convertFunctionSymbol fs

convertFunctionSymbol :: SrcFunctionSymbol -> DstFunctionSymbol
convertFunctionSymbol (Ari.Sig name arity) = symbol name arity
