{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Parser where
import Text.Parsec.String
import Header
import Text.Parsec (string, many, lower, alphaNum, (<|>), char, many1, optionMaybe, digit, parse)
import Control.Monad (void)
import Prelude hiding (fail)
import Data.Foldable (foldl')

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
    return (first:rest)

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
    t2 <- (case mb_arrow of
        Just _ -> ArrowSyntax t <$> parseType
        Nothing -> return t)
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
    e <- parseLambda <|> parenthetical parseExpr <|> parseVar <|> parseLit
    ws
    es <- many $ (Left <$> (ws *> parenthetical parseExpr <* ws)) <|> (Right <$> (ws *> bracketed parseType <* ws))
    let e2 = foldl' f e es
    ws
    return e2
    where
        f e (Left e2) = AppSyntax e e2
        f e (Right t) = TAppSyntax e t