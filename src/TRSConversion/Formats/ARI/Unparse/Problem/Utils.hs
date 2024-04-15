{-# LANGUAGE AllowAmbiguousTypes #-}
module TRSConversion.Formats.ARI.Unparse.Problem.Utils (unparseIdentifier) where
import Prettyprinter ( Doc, Pretty(pretty) )
import TRSConversion.Formats.ARI.Parse.Utils (isIdentChar)

unparseIdentifier :: Pretty f => f -> Doc ann
unparseIdentifier identifier =
    let str = pretty identifier in
    if all isIdentChar $ show str then str else pretty "|" <> str <> pretty "|"
