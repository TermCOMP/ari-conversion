{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

-- |
-- Module      : Data.Conversion.Parse.Problem.MetaInfo
-- Description : Comment parser
--
-- This module defines parsers to parse the additional information (comment, author, etc.) of a
-- given rewriting system.
module Data.Conversion.Parse.Problem.MetaInfo
  ( -- * COPS
    parseCopsMetaInfo,

    -- * ARI
    parseAriMetaInfo,
  )
where

import Data.Char (isSpace)
import Data.Text (unpack, Text)
import Text.Megaparsec (MonadParsec (notFollowedBy), between, noneOf, option, sepEndBy, many, some, takeWhile1P, takeWhileP, try, (<?>), (<|>), sepBy1, satisfy, region)
import Text.Megaparsec.Char (char, hspace, space, string, hspace1)

import Data.Conversion.Parse.Utils (Parser)
import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo, mergeMetaInfo)
import Data.Foldable (foldl')
import qualified Text.Megaparsec.Error as E
import Data.List.NonEmpty (NonEmpty((:|)))

-- | Parser to extract comments as a @String@ from the (optional) @COMMENT@ block of the COPS TRS format.
--
parseCopsMetaInfo :: Parser MetaInfo
parseCopsMetaInfo = do
  ls <- sepEndBy (metaDoi <|> metaAuthors <|> metaComment) space
  return $ foldr mergeMetaInfo emptyMetaInfo ls
 where
   metaDoi = (\v -> emptyMetaInfo {doi = Just v}) <$> copsDoiLine
   metaAuthors = (\v -> emptyMetaInfo {submitted = Just v}) <$> copsAuthorsLine
   metaComment = (\v -> emptyMetaInfo {comment = Just [v]}) <$> copsCommentLine

copsDoiLine :: Parser String
copsDoiLine = "doi:" *> (pDoi <?> "doi")
  where
    pDoi = unpack <$> takeWhileP Nothing (not . isSpace)

copsAuthorsLine :: Parser [String]
copsAuthorsLine = "submitted by:" *> hspace *> (pAuthors <?> "authors")
  where
    pAuthors =
      sepBy1
        (some . try $ (char 'a' <* notFollowedBy (string "nd" <* hspace1)) <|> satisfy (\c -> c /= ',' && c /= '\n'))
        (some $ (string "," <|> string "and") <* hspace)

copsCommentLine :: Parser String
copsCommentLine = do
  pref <- unpack <$> takeWhile1P (Just "comment") (\c -> c `notElem` ['\n','\r','(',')'])
  rest <- option "" $ do
    par <- between (char '(') (char ')') parseComment
    suf <- option "" copsCommentLine
    pure $ '(' : par ++ ')': suf
  pure $ pref ++ rest


-- | Parser to parse a string comment between two outermost parentheses.
-- Calls itself recursively to allow for nested parentheses inside comments.
--
-- The recursive logic is adapted from the library @term-rewriting@.
parseComment :: Parser String
parseComment =
  withParens
    <|> (++) <$> some (noneOf ['(', ')']) <*> parseComment
    <|> return ""
  where
    withParens :: Parser String
    withParens = do
      _ <- char '('
      pre <- parseComment
      _ <- char ')'
      suf <- parseComment
      return $ "(" ++ pre ++ ")" ++ suf

-- | Parse meta-info blocks in ARI format
--
-- If multiple DOIs are specified only the first is used.
--
-- Expects e.g. something like
-- @
-- ; @origin COPS #20
-- ; @doi 10.1007/11805618_6
-- ; @author Takahito Aoto
-- ; @author Junichi Yoshida
-- ; @author Yoshihito Toyama
-- ; [7] Example 2
-- @.
--
parseAriMetaInfo :: Parser MetaInfo
parseAriMetaInfo = do
  meta <- foldl' mergeMetaInfo emptyMetaInfo <$> many structuredMeta
  comments <- foldl' mergeMetaInfo emptyMetaInfo <$> many ariLeadingComment
  pure $ mergeMetaInfo meta comments
  where
    structuredMeta = ariAuthorLine <|> ariDoiLine

metaKeyValue :: Text -> Parser Text
metaKeyValue key =
   between
    (try (string "; " *> region shapeErr (string ("@" <> key) <* char ' ')))
    (char '\n')
    (takeWhileP (Just "character") (\c -> c `notElem` ['\r','\n']))
 where
   shapeErr (E.TrivialError o (Just (E.Tokens ('@' :| tl))) expS)
            = E.TrivialError o (Just (E.Tokens ('@' :| takeWhile (not . isSpace) tl))) expS
   shapeErr e = e


ariDoiLine :: Parser MetaInfo
ariDoiLine = do
  doiStr <- metaKeyValue "doi"
  pure $ emptyMetaInfo {doi = Just $ unpack doiStr}

ariAuthorLine :: Parser MetaInfo
ariAuthorLine = do
  doiStr <- metaKeyValue "author"
  pure $ emptyMetaInfo {submitted = Just [unpack doiStr]}

ariLeadingComment :: Parser MetaInfo
ariLeadingComment = between (char ';') (char '\n') $ do
  notFollowedBy (string " @")
  cmt <- takeWhileP (Just "character") (/= '\n')
  pure $ emptyMetaInfo {comment = Just [unpack cmt]}
