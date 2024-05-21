{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License v. 2.0. If a copy of the MPL was not distributed with this
   file You can obtain one at https:--mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE TupleSections #-}

module Regions (go) where

import Header
import GHC.IO (unsafePerformIO)

go :: [StmtA] -> SLC [StmtR]
go stmts = do
  stmts2 <- mapM goStmt stmts
  return $ unsafePerformIO (writeFile "t.gleam" (unlines $ map pretty stmts2) >> return id) stmts2

goStmt :: StmtA -> SLC StmtR
goStmt (FuncA x s tvars params body) = do
  r <- fresh
  rh <- fresh
  let r_ident = Local r "r"
  let rh_ident = Local rh "h"
  params2 <- mapM (\(a, s2, b) -> (a,s2,) <$> goType r_ident rh_ident b) params
  body2 <- goExpr r_ident rh_ident body
  return $ FuncR x s ((r, True, "r") : map (\(a,b)->(a,False,b)) tvars) ((rh, "h", HandleTypeR $ Local r "r") : params2) body2

goExpr :: Ident -> Ident -> ExprA -> SLC ExprR
goExpr r rh e = case e of
  AppA f args -> do
    f2 <- goVal r rh f
    args2 <- mapM (goVal r rh) args
    return $ AppR f2 (VarR (HandleTypeR r) False rh : args2)
  HaltA v -> HaltR <$> goVal r rh v
  ProjA x s t tpl i scope -> do
    tpl2 <- goVal r rh tpl
    scope2 <- goExpr r rh scope
    t2 <- goType r rh t
    return $ ProjR x s t2 tpl2 i scope2
  UnpackA x s1 y s2 v scope -> do
    v2 <- goVal r rh v
    scope2 <- goExpr r rh scope
    return $ UnpackR x s1 y s2 v2 scope2
  MallocA x s ts scope -> do
    ts2 <- mapM (goType r rh) ts
    scope2 <- goExpr r rh scope
    return $ MallocR x s ts2 r rh scope2
  InitA x s tpl i v scope -> do
    tpl2 <- goVal r rh tpl
    v2 <- goVal r rh v
    scope2 <- goExpr r rh scope
    return $ InitR x s tpl2 i v2 scope2

goVal :: Ident -> Ident -> ValA -> SLC ValR
goVal r rh v = case v of
  IntLitA i -> return $ IntLitR i
  VarA t global idNum -> do
    t2 <- goType r rh t
    if global then
      return $ RegionAppR (VarR t2 global idNum) [r]
    else
      return $ VarR t2 global idNum
  TypeLambdaA params body -> do
    body2 <- goVal r rh body
    return $ TypeLambdaR params body2
  TypeAppA t f ts -> do
    t2 <- goType r rh t
    f2 <- goVal r rh f
    ts2 <- mapM (goType r rh) ts
    return $ TypeAppR t2 f2 ts2
  PackA t v2 t2 -> do
    t' <- goType r rh t
    v2' <- goVal r rh v2
    t2' <- goType r rh t2
    return $ PackR t' v2' t2'

goType :: Ident -> Ident -> TypeA -> SLC TypeR
goType r rh t = case t of
  I32A -> return I32R
  TypeVarA idNum -> return $ TypeVarR idNum
  ForallA params body -> ForallR params <$> goType r rh body
  FunctionTypeA params -> do
    params2 <- mapM (goType r rh) params
    return $ FunctionTypeR (HandleTypeR r : params2)
  TupleTypeA ts -> do
    ts2 <- mapM (goType r rh) ts
    return $ TupleTypeR r ts2
  ExistentialA x s body -> do
    body2 <- goType r rh body
    return $ ExistentialR x s body2

-- subR :: Ident -> Ident -> TypeR -> TypeR
-- subR old_r new_r t = case t of
--   I32R -> I32R
--   TypeVarR ident -> TypeVarR ident
--   ForallR params body -> ForallR params (subR old_r new_r body)
--   ForallRegionR params body -> ForallRegionR params (subR old_r new_r body)
--   FunctionTypeR params -> FunctionTypeR (map (subR old_r new_r) params)
--   TupleTypeR r ts -> 
--     let ts2 = map (subR old_r new_r) ts in
--     if r == old_r then TupleTypeR new_r ts2 else TupleTypeR r ts2
--   ExistentialR idNum s body -> ExistentialR idNum s (subR old_r new_r body)
--   HandleTypeR r -> if r == old_r then HandleTypeR new_r else HandleTypeR r