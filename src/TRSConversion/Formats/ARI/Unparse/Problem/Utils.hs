{-# LANGUAGE AllowAmbiguousTypes #-}
module TRSConversion.Formats.ARI.Unparse.Problem.Utils (unparseIdentifier) where
import Prettyprinter ( Doc, Pretty(pretty) )
import TRSConversion.Formats.ARI.Parse.Utils (isIdentChar)
import Data.Char (isDigit)

ariKeywords :: [String]
ariKeywords = ["fun", "rule", "format", "sort", "theory", "define-fun"]

unparseIdentifier :: Pretty f => f -> Doc ann
unparseIdentifier identifier =
    let ident = pretty identifier in
    let str = show ident in
    if all isIdentChar str && not (isDigit $ head str) && str `notElem` ariKeywords then ident else pretty "|" <> ident <> pretty "|"
