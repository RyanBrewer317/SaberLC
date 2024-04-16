{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Alloc (go) where

import Control.Monad (mapAndUnzipM)
import Data.Bifunctor (second)
import Header
import Prelude hiding (id)

data Decl = Malloc Int [TypeA] | Init Int ValA Int ValA

go :: [StmtH] -> SLC [StmtA]
go = mapM goStmt

goStmt :: StmtH -> SLC StmtA
goStmt (FuncH id tvars params body) = FuncA id tvars (map (second goType) params) <$> goExpr body

goExpr :: ExprH -> SLC ExprA
goExpr e = case e of
  HaltH v -> do
    (v2, decls) <- goVal v
    return $ putDecls decls $ HaltA v2
  AppH f as -> do
    (f2, decls) <- goVal f
    (as2, decls2) <- mapAndUnzipM goVal as
    return $ putDecls (decls ++ concat decls2) $ AppA f2 as2
  ProjH x t tpl i scope -> do
    (tpl2, decls) <- goVal tpl
    scope2 <- goExpr scope
    return $ putDecls decls $ ProjA x (goType t) tpl2 i scope2
  UnpackH t x v scope -> do
    (v2, decls) <- goVal v
    scope2 <- goExpr scope
    return $ putDecls decls $ UnpackA t x v2 scope2

putDecls :: [Decl] -> ExprA -> ExprA
putDecls decls e =
  foldr
    ( \d e2 ->
        case d of
          Malloc x ts -> MallocA x ts e2
          Init x tpl i v2 -> InitA x tpl i v2 e2
    )
    e
    decls

goVal :: ValH -> SLC (ValA, [Decl])
goVal v = case v of
  IntLitH lit -> return (IntLitA lit, [])
  VarH t global x -> return (VarA (goType t) global x, [])
  TupleH vs -> do
    (vs2, decls) <- mapAndUnzipM goVal vs
    x <- fresh
    let xt = goType $ typeOfHVal v
    let first_var = VarA xt False $ Local x
    (end, decls2) <- getDecls vs2 0 first_var xt
    return (end, concat decls ++ Malloc x (map typeOfAVal vs2) : decls2)
  TypeLambdaH xs body -> do
    (body2, decls) <- goVal body
    return (TypeLambdaA xs body2, decls)
  TypeAppH t v2 ts -> do
    (v3, decls) <- goVal v2
    return (TypeAppA (goType t) v3 (map goType ts), decls)
  PackH t v2 t2 -> do
    (v3, decls) <- goVal v2
    return (PackA (goType t) v3 (goType t2), decls)

getDecls :: [ValA] -> Int -> ValA -> TypeA -> SLC (ValA, [Decl])
getDecls [] _ v _ = return (v, [])
getDecls (v : vs) i last_var t = do
  x <- fresh
  let decl = Init x last_var i v
  (end, rest) <- getDecls vs (i + 1) (VarA t False $ Local x) t
  return (end, decl : rest)

goType :: TypeH -> TypeA
goType t = case t of
  I32H -> I32A
  TypeVarH tv -> TypeVarA tv
  ForallH xs body -> ForallA xs (goType body)
  FunctionTypeH ts -> FunctionTypeA (map goType ts)
  TupleTypeH xs -> TupleTypeA (map goType xs)
  ExistentialH x t2 -> ExistentialA x (goType t2)

typeOfHVal :: ValH -> TypeH
typeOfHVal v = case v of
    IntLitH _ -> I32H
    VarH t _ _ -> t
    TupleH vs -> TupleTypeH (map typeOfHVal vs)
    TypeLambdaH xs body -> ForallH xs (typeOfHVal body)
    TypeAppH t _ _ -> t
    PackH _ _ t2 -> t2

typeOfAVal :: ValA -> TypeA
typeOfAVal v = case v of
    IntLitA _ -> I32A
    VarA t _ _ -> t
    TupleA vs -> TupleTypeA (map typeOfAVal vs)
    TypeLambdaA xs body -> ForallA xs (typeOfAVal body)
    TypeAppA t _ _ -> t
    PackA _ _ t2 -> t2