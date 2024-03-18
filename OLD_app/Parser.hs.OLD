{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Parser (go) where

import Control.Monad (void)
import Data.Foldable (foldl')
import Header
import Text.Parsec (alphaNum, char, digit, lower, many, many1, optionMaybe, parse, string, (<|>))
import Text.Parsec.String
import Prelude hiding (fail)

go :: String -> SLC ExprSyntax
go s = case parse parseExpr "INPUT" s of
  Left err -> fail $ ParseError $ show err
  Right x -> return x

parseI32 :: Parser TypeSyntax
parseI32 = do
  _ <- string "i32"
  return I32Syntax

parseIdentifier :: Parser String
parseIdentifier = do
  first <- lower
  rest <- many (alphaNum <|> char '_')
  return (first : rest)

parseTVar :: Parser TypeSyntax
parseTVar = TVarSyntax <$> parseIdentifier

ws1 :: Parser ()
ws1 = void (many1 (char ' ' <|> char '\t' <|> char '\n'))

ws :: Parser ()
ws = void (many (char ' ' <|> char '\t' <|> char '\n'))

parseForall :: Parser TypeSyntax
parseForall = do
  _ <- string "forall"
  ws1
  x <- parseIdentifier
  ws
  _ <- char '.'
  ForallSyntax x <$> parseType

parseType :: Parser TypeSyntax
parseType = do
  ws
  t <- parseForall <|> parseI32 <|> parseTVar
  ws
  mb_arrow <- optionMaybe (string "->")
  t2 <-
    ( case mb_arrow of
        Just _ -> ArrowSyntax t <$> parseType
        Nothing -> return t
      )
  ws
  return t2

parseLambda :: Parser ExprSyntax
parseLambda = do
  _ <- char '\\'
  ws
  x <- parseIdentifier
  ws
  _ <- char ':'
  ws
  t <- parseType
  ws
  _ <- char '.'
  ws
  LambdaSyntax x t <$> parseExpr

parseTLambda :: Parser ExprSyntax
parseTLambda = do
  _ <- string "/\\"
  ws
  x <- parseIdentifier
  ws
  _ <- char '.'
  ws
  TLambdaSyntax x <$> parseExpr

parseVar :: Parser ExprSyntax
parseVar = VarSyntax <$> parseIdentifier

parseLit :: Parser ExprSyntax
parseLit = LitSyntax . read <$> many1 digit

parenthetical :: Parser a -> Parser a
parenthetical p = char '(' *> ws *> p <* ws <* char ')'

bracketed :: Parser a -> Parser a
bracketed p = char '[' *> ws *> p <* ws <* char ']'

parseExpr :: Parser ExprSyntax
parseExpr = do
  ws
  e <- parseLambda <|> parseTLambda <|> parenthetical parseExpr <|> parseVar <|> parseLit
  ws
  es <- many $ (Left <$> (ws *> parenthetical parseExpr <* ws)) <|> (Right <$> (ws *> bracketed parseType <* ws))
  let e2 = foldl' f e es
  ws
  return e2
  where
    f e (Left e2) = AppSyntax e e2
    f e (Right t) = TAppSyntax e t