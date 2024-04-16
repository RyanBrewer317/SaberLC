{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Hoist (go) where

import Control.Monad (mapAndUnzipM, (>=>))
import Data.Bifunctor (bimap, first, second)
import Header

go :: ExprCC -> SLC [StmtH]
go = goExpr >=> \(e, stmts) -> return $ FuncH (-1) [] [] e : stmts

goExpr :: ExprCC -> SLC (ExprH, [StmtH])
goExpr e = case e of
  HaltCC v -> first HaltH <$> goVal v
  AppCC f as -> goVal f >>= \(f2, stmts) -> bimap (AppH f2) ((stmts ++) . concat) <$> mapAndUnzipM goVal as
  ProjCC x t tpl i scope -> do
    (v2, stmts) <- goVal tpl
    (scope2, stmst2) <- goExpr scope
    return (ProjH x (goType t) v2 i scope2, stmts ++ stmst2)
  UnpackCC x y v scope -> do
    (v2, stmts) <- goVal v
    (scope2, stmts2) <- goExpr scope
    return (UnpackH x y v2 scope2, stmts ++ stmts2)

goVal :: ValCC -> SLC (ValH, [StmtH])
goVal v = case v of
  IntLitCC i -> return (IntLitH i, [])
  VarCC t global x -> return (VarH (goType t) global x, [])
  f@(LambdaCC xs e) -> do
    x <- fresh
    (e2, stmts) <- goExpr e
    return (VarH (goType $ typeOfCCVal f) True $ Local x, FuncH x [] (map (second goType) xs) e2 : stmts)
  TupleCC xs -> bimap TupleH concat <$> mapAndUnzipM (goVal >=> (\ (x2, stmts) -> return (x2, stmts))) xs
  TypeLambdaCC xs body -> do
    (body2, stmts) <- goVal body
    return (TypeLambdaH xs body2, stmts)
  TypeAppCC t e ts -> goVal e >>= \(e2, stmts) -> return (TypeAppH (goType t) e2 (map goType ts), stmts)
  PackCC t v2 t2 -> goVal v2 >>= \(v3, stmts) -> return (PackH (goType t) v3 (goType t2), stmts)

goType :: TypeCC -> TypeH
goType t = case t of
  I32CC -> I32H
  TypeVarCC x -> TypeVarH x
  ForallCC x t2 -> ForallH x (goType t2)
  FunctionTypeCC xs -> FunctionTypeH (map goType xs)
  TupleTypeCC ts -> TupleTypeH (map goType ts)
  ExistialCC x t2 -> ExistentialH x (goType t2)

typeOfCCVal :: ValCC -> TypeCC
typeOfCCVal v = case v of
    IntLitCC _ -> I32CC
    VarCC t _ _ -> t
    LambdaCC xs _ -> FunctionTypeCC $ map snd xs
    TupleCC xs -> TupleTypeCC $ map typeOfCCVal xs
    TypeLambdaCC xs body -> ForallCC xs (typeOfCCVal body)
    TypeAppCC t _ _ -> t
    PackCC _ _ t -> t