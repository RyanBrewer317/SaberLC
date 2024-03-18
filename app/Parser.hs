{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}

module Parser (go) where

import Header
import Text.Parsec (alphaNum, char, digit, lower, many, many1, optionMaybe, parse, string, (<|>))
import Text.Parsec.String (Parser)
import Data.Foldable (foldl')

go :: String -> SLC ExprParse
go s = case parse parseExpr "INPUT" s of
  Left err -> throw $ ParseError $ show err
  Right x -> return x

parseI32 :: Parser TypeParse
parseI32 = do
  _ <- string "i32"
  return I32Parse

parseIdentifier :: Parser String
parseIdentifier = do
  first <- lower
  rest <- many (alphaNum <|> char '_')
  return (first : rest)

parseTVar :: Parser TypeParse
parseTVar = TypeVarParse <$> parseIdentifier

ws1 :: Parser ()
ws1 = many1 (char ' ' <|> char '\t' <|> char '\n') >> return ()

ws :: Parser ()
ws = many (char ' ' <|> char '\t' <|> char '\n') >> return ()

parseForall :: Parser TypeParse
parseForall = do
  _ <- string "forall"
  ws1
  x <- parseIdentifier
  ws
  _ <- char '.'
  ForallParse x <$> parseType

parseType :: Parser TypeParse
parseType = do
  ws
  t <- parseForall <|> parseI32 <|> parseTVar
  ws
  mb_arrow <- optionMaybe (string "->")
  t2 <-
    ( case mb_arrow of
        Just _ -> FunctionTypeParse t <$> parseType
        Nothing -> return t
      )
  ws
  return t2

parseLambda :: Parser ExprParse
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
  LambdaParse x t <$> parseExpr

parseTLambda :: Parser ExprParse
parseTLambda = do
  _ <- string "/\\"
  ws
  x <- parseIdentifier
  ws
  _ <- char '.'
  ws
  TypeLambdaParse x <$> parseExpr

parseVar :: Parser ExprParse
parseVar = VarParse False <$> parseIdentifier

parseLit :: Parser ExprParse
parseLit = IntLitParse . read <$> many1 digit

parenthetical :: Parser a -> Parser a
parenthetical p = char '(' *> ws *> p <* ws <* char ')'

bracketed :: Parser a -> Parser a
bracketed p = char '[' *> ws *> p <* ws <* char ']'

parseExpr :: Parser ExprParse
parseExpr = do
  ws
  e <- parseLambda <|> parseTLambda <|> parenthetical parseExpr <|> parseVar <|> parseLit
  ws
  es <- many $ (Left <$> (ws *> parenthetical parseExpr <* ws)) <|> (Right <$> (ws *> bracketed parseType <* ws))
  let e2 = foldl' f e es
  ws
  return e2
  where
    f e (Left e2) = AppParse e e2
    f e (Right t) = TypeAppParse e t