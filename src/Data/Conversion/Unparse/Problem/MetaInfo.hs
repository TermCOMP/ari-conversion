-- |
-- Module      : Data.Conversion.Unparse.Problem.MetaInfo
-- Description : Unparser for MetaInfo
--
-- This module defines functions to unparse 'MetaInfo' into the formats expected for the COPS and ARI formats.
module Data.Conversion.Unparse.Problem.MetaInfo
  ( unparseCopsMetaInfo,
    unparseAriMetaInfo,
  )
where

import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..))
import Data.Conversion.Unparse.Utils (filterEmptyDocs, prettyBlock)
import Prettyprinter (Doc, comma, emptyDoc, hsep, parens, pretty, punctuate, vsep, (<+>))

-- | Unparse TRS 'MetaInfo' to fit into a single COPS @COMMENT@ block.
-- If the 'MetaInfo' is empty then returns emptyDoc.
--
-- qqjf I was unsure what format is expected but this is easy to adjust.
unparseCopsMetaInfo :: MetaInfo -> Doc ann
unparseCopsMetaInfo (MetaInfo cs ds orig sub) =
  if null metaBlocks
    then emptyDoc
    else prettyBlock "COMMENT" (vsep $ emptyDoc : metaBlocks)
  where
    metaBlocks :: [Doc ann]
    metaBlocks =
      filterEmptyDocs [maybe emptyDoc (\d -> pretty "doi:" <> pretty d) ds]
        ++ maybe [] (\c -> [pretty c]) cs
        ++ filterEmptyDocs
          [ maybe emptyDoc (\org -> pretty "origin:" <+> pretty org) orig,
            maybe emptyDoc unparseSubmitters sub
          ]
    -- Unparse submitters as a comma-separated list
    unparseSubmitters :: [String] -> Doc ann
    unparseSubmitters xs = pretty "submitted by:" <+> hsep (punctuate comma $ map pretty xs)

-- | Unparse 'MetaInfo' into ARI format. See the tests for more examples.
--
-- >>> unparseAriMetaInfo $ emptyMetaInfo { comment = Just "[7] Example 2", doi = Just "10.1007/11805618_6", origin = Just "COPS #20",submitted = Just ["Takahito Aoto", "Junichi Yoshida", "Yoshihito Toyama"] }
-- (meta-info (origin "COPS #20"))
-- (meta-info (doi "10.1007/11805618_6"))
-- (meta-info (comment "[7] Example 2"))
-- (meta-info (submitted "Takahito Aoto" "Junichi Yoshida" "Yoshihito Toyama"))
--
-- >>> unparseAriMetaInfo emptyMetaInfo
-- Nothing
unparseAriMetaInfo :: MetaInfo -> Maybe (Doc ann)
unparseAriMetaInfo (MetaInfo cs ds orig sub) =
  if null metaBlocks
    then Nothing
    else Just $ vsep metaBlocks
  where
    metaBlocks :: [Doc ann]
    metaBlocks =
      filterEmptyDocs
        [ maybe emptyDoc (metaBlock "origin") orig,
          maybe emptyDoc (metaBlock "doi") ds,
          maybe emptyDoc (metaBlock "comment") cs,
          maybe emptyDoc unparseSubmitters sub
        ]

    -- Unparse submitters as a space-separated list of strings
    unparseSubmitters :: [String] -> Doc ann
    unparseSubmitters xs = metaBlock "submitted" (hsep $ map (pretty . show) xs)
    metaBlock :: Show a => String -> a -> Doc ann
    metaBlock name xs = parens $ pretty "meta-info" <+> parens (pretty name <+> pretty (show xs))
