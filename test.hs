{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Conversion.Problem.Term.Parse
  ( parseTerm,
    parseVariable,
  )
where

import Control.Monad (guard)
import Data.Rewriting.Term.Type
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    anySingle,
    between,
    choice,
    eof,
    errorBundlePretty,
    fancyFailure,
    lookAhead,
    many,
    manyTill_,
    noneOf,
    parse,
    parseTest,
    satisfy,
    sepBy,
    skipMany,
    some,
    try,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char (char, letterChar, space1, spaceChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ErrorFancy (..), errorBundlePretty)
import Prelude hiding (lex)

type Parser = Parsec Void Text

type Vars = [String]

sc :: Parser ()
sc =
  L.space
    space1 -- (2)
    (L.skipLineComment "//") -- (3)
    (L.skipBlockComment "/*" "*/") -- (4)

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | lexeme is a lexeme that consumes all trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a term given a list of variables by calling 'parseVariable' and 'parseFunApplication'
-- Tries to parse the expression as a variable first
parseTerm :: Vars -> Parser (Term String String)
parseTerm vs =
  stripSpaces $
    choice
      [ -- stripSpaces $ parens (parseTerm vs),
        {-stripSpaces-} parseTermHelper
      ]
  where
    -- \| Try to parse the given string as a variable, then as a function application, then as a constant
    parseTermHelper :: Parser (Term String String)
    parseTermHelper =
      try (parseVariable vs)
        <|> try (parseFunApplication vs)
        <|> try parseConstant
    -- <|> try parseDebug
    parseDebug :: Parser (Term String String)
    parseDebug = do
      input <- many anySingle
      return (Var (input ++ " DEBUG"))

    parseConstant :: Parser (Term String String)
    parseConstant = do
      input <- parseFunSymbol
      return (Fun input [])

-- | Strip outer parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")" *> eof)

-- | Parse a single variable
--   Currently requires the first character to be a letter
-- Uses @lexeme@ to comsume trailing whitespace
parseVariable :: Vars -> Parser (Term f String)
parseVariable vs = do
  varStr <- (:) <$> letterChar <*> lexeme (many allowedFunVarChars) <?> "variable"
  guard (varStr `elem` vs)
  return (Var varStr)

-- | Parse a function application and return a 'Term'
-- Assumes that everything until the first @'('@ is a function symbol
-- For example, "f(x,y, g(z))"
parseFunApplication :: Vars -> Parser (Term String String)
parseFunApplication vs =
  do
    fsym <- parseFunSymbol
    argsStr <- stripOuterParens -- Remove outer parentheses
    -- input <- many anySingle
    -- return (Var input)
    case parse (parseFunArgs vs <* eof) "" (pack argsStr) of
      Left err -> fancyFailure (Set.singleton (ErrorFail (errorBundlePretty err)))
      Right args -> return (Fun fsym args)

-- | Strip spaces at start and end of a string
stripSpaces :: Parser a -> Parser a
stripSpaces p = lexeme (many spaceChar *> p)

-- | Recursively strip outer parentheses of a function application, even if nested
-- Done in a slightly weird way with megaparsec as this library expects to always process streams from front to back
stripOuterParens :: Parser String
stripOuterParens = do
  out <- aux
  let output = case parse stripOuterParens "" (pack out) of
        Left _ -> out -- Original output was ok
        Right xs -> xs
  return output
  where
    aux :: Parser String
    aux = do
      (noParens, _) <- char '(' *> manyTill_ anySingle (try (lexeme (char ')') <* eof))
      -- (noParens, _) <- char '(' *> manyTill_ anySingle (try (char ')' <* eof))
      return noParens
    manyTillParen :: Parser (String, Char)
    manyTillParen = char '(' *> manyTill_ anySingle (try (lexeme (char ')') <* eof))

-- | Parse a function symbol either until the first '(' or as long as allowed characters are there
-- Important: does not consume all input
parseFunSymbol :: Parser String
parseFunSymbol =
  try
    (funSymChars <* lookAhead (char '('))
    <|> funSymChars
    <?> "function symbol"
  where
    funSymChars :: Parser String
    funSymChars = lexeme (some allowedFunVarChars)

-- | Parse function arguments
-- Expects balanced parentheses and comma-separated values
-- Returns a list of the arguments. Also accepts an empty string or just whitespace, corresponding to an empty arguments list.
-- e.g. "(a, b, c)" or "(a, b(c))"
parseFunArgs :: Vars -> Parser [Term String String]
parseFunArgs vs = parseTerm vs `sepBy` char ',' <* eof <?> "function arguments"

-- | Parser for characters allowed after the first character of variables and for function symbols.
--   Currently allows any character except for '(', ')', ',', and whitespace.
--   TODO: block all whitespace and special characters, not just a single space
allowedFunVarChars :: Parser Char
allowedFunVarChars = noneOf ['(', ')', ' ', ',']

t0 = parseTest (parseTerm ["x", "y"] <* eof) "f(c,y,z)"

t02 = parseTest (parseTerm ["x", "y"] <* eof) " "

t03 = parseTest (parseTerm ["x", "y"] <* eof) "c "

t04 = parseTest (parseTerm ["x", "y"] <* eof) "x "

-- t05 = parseTest (lexeme (many (noneOf [' '])) <* eof) "f(c,y,z) "
-- t06 = parseTest (stripSpaces (many (noneOf [' '])) <* eof) "f(c,y,z) "

t1 = parseTest (parseTerm ["x", "y"] <* eof) "f(c,y,z) "

t15 = parseTest (lexeme (parseTerm ["x", "y"]) <* eof) "f(c,y,z) "

t2 = parseTest (parseFunArgs ["x", "y"] <* eof) "f(x,y,z)"

t3 = parseTest (parseFunArgs ["x", "y"] <* eof) "f(x,y,z) "

t4 = parseTest (stripSpaces (parseTerm ["x", "y"] <* eof)) " c"

t5 = parseTest (parseTerm ["x", "y"] <* eof) " c"