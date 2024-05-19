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

data Decl = Malloc Int String [TypeA] | Init Int String ValA Int ValA

go :: [StmtH] -> SLC [StmtA]
go = mapM goStmt

goStmt :: StmtH -> SLC StmtA
goStmt (FuncH id s tvars params body) = FuncA id s tvars (map (second goType) params) <$> goExpr body

goExpr :: ExprH -> SLC ExprA
goExpr e = case e of
  HaltH v -> do
    (v2, decls) <- goVal v
    return $ putDecls decls $ HaltA v2
  AppH f as -> do
    (f2, decls) <- goVal f
    (as2, decls2) <- mapAndUnzipM goVal as
    return $ putDecls (decls ++ concat decls2) $ AppA f2 as2
  ProjH x s t tpl i scope -> do
    (tpl2, decls) <- goVal tpl
    scope2 <- goExpr scope
    return $ putDecls decls $ ProjA x s (goType t) tpl2 i scope2
  UnpackH t s1 x s2 v scope -> do
    (v2, decls) <- goVal v
    scope2 <- goExpr scope
    return $ putDecls decls $ UnpackA t s1 x s2 v2 scope2

putDecls :: [Decl] -> ExprA -> ExprA
putDecls decls e =
  foldr
    ( \d e2 ->
        case d of
          Malloc x s ts -> MallocA x s ts e2
          Init x s tpl i v2 -> InitA x s tpl i v2 e2
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
    let first_var = VarA xt False $ Local x "tpl"
    (end, decls2) <- getDecls "tpl" vs2 0 first_var xt
    return (end, concat decls ++ Malloc x "tpl" (map typeOfAVal vs2) : decls2)
  TypeLambdaH xs body -> do
    (body2, decls) <- goVal body
    return (TypeLambdaA xs body2, decls)
  TypeAppH t v2 ts -> do
    (v3, decls) <- goVal v2
    return (TypeAppA (goType t) v3 (map goType ts), decls)
  PackH t v2 t2 -> do
    (v3, decls) <- goVal v2
    return (PackA (goType t) v3 (goType t2), decls)

getDecls :: String -> [ValA] -> Int -> ValA -> TypeA -> SLC (ValA, [Decl])
getDecls _ [] _ v _ = return (v, [])
getDecls s (v : vs) i last_var t = do
  x <- fresh
  let decl = Init x s last_var i v
  (end, rest) <- getDecls s vs (i + 1) (VarA t False $ Local x s) t
  return (end, decl : rest)

goType :: TypeH -> TypeA
goType t = case t of
  I32H -> I32A
  TypeVarH tv -> TypeVarA tv
  ForallH xs body -> ForallA xs (goType body)
  FunctionTypeH ts -> FunctionTypeA (map goType ts)
  TupleTypeH xs -> TupleTypeA (map goType xs)
  ExistentialH x s t2 -> ExistentialA x s (goType t2)

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
    TypeLambdaA xs body -> ForallA xs (typeOfAVal body)
    TypeAppA t _ _ -> t
    PackA _ _ t2 -> t2