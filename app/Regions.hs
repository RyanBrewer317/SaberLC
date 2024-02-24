{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License v. 2.0. If a copy of the MPL was not distributed with this
   file You can obtain one at https:--mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module Regions where

import Header
import Data.Void (absurd)
import Prelude hiding (id)

go :: [Stmt U V] -> SLC [Stmt U U]
go = mapM goStmt

goStmt :: Stmt U V -> SLC (Stmt U U)
goStmt (Func x tvars params body) = do
    r <- fresh
    rh <- fresh
    let tvars2 = map goEntry tvars
    params2 <- mapM (\(a,b)->(a,)<$>goType r rh b) params
    body2 <- goExpr r rh body
    return $ Func x (RgnEntry r : tvars2) ((rh, HandleTypeCPS () r):params2) body2

goExpr :: Id -> Id -> ExprCPS U U U V -> SLC (ExprCPS U U U U)
goExpr r rh e = case e of
    AppCPS f targs args -> do
        f2 <- goVal r rh f
        targs2 <- mapM (goType r rh) targs
        args2 <- mapM (goVal r rh) args
        return $ AppCPS f2 targs2 args2
    HaltCPS v -> HaltCPS <$> goVal r rh v
    LetCPS x t v scope -> do
        t2 <- goType r rh t
        v2 <- goVal r rh v
        scope2 <- goExpr r rh scope
        return $ LetCPS x t2 v2 scope2
    TupleProjCPS x tpl i scope -> do
        tpl2 <- goVal r rh tpl
        scope2 <- goExpr r rh scope
        return $ TupleProjCPS x tpl2 i scope2
    UnpackCPS () x y v scope -> do
        v2 <- goVal r rh v
        scope2 <- goExpr r rh scope
        return $ UnpackCPS () x y v2 scope2
    MallocCPS () x ts NotAssignedRgn scope -> do
        ts2 <- mapM (goType r rh) ts
        scope2 <- goExpr r rh scope
        return $ MallocCPS () x ts2 (Rgn rh) scope2
    InitCPS () x tpl i v scope -> do
        tpl2 <- goVal r rh tpl
        v2 <- goVal r rh v
        scope2 <- goExpr r rh scope
        return $ InitCPS () x tpl2 i v2 scope2

goVal :: Id -> Id -> ValCPS U U U V -> SLC (ValCPS U U U U)
goVal r rh v = case v of
    LitCPS i -> return $ LitCPS i
    VarCPS id t global -> do
        t2 <- goType r rh t
        return $ VarCPS id t2 global
    LambdaCPS (NotTrue void) _ _ _ -> absurd void
    TupleCPS _ (NotTrue void) _ -> absurd void
    TAppCPS () t f ts -> do
        t2 <- goType r rh t
        f2 <- goVal r rh f
        ts2 <- mapM (goType r rh) ts
        return $ TAppCPS () t2 f2 ts2
    PackCPS () t v2 t2 -> do
        t' <- goType r rh t
        v2' <- goVal r rh v2
        t2' <- goType r rh t2
        return $ PackCPS () t' v2' t2'

goType :: Id -> Id -> TypeCPS U U U V -> SLC (TypeCPS U U U U)
goType r rh t = case t of
    I32CPS -> return I32CPS
    TVarCPS id -> return $ TVarCPS id
    ArrowCPS tvars params -> do
        r2 <- fresh
        params2 <- mapM (goType r rh) params
        return $ ArrowCPS (RgnEntry r2 : map goEntry tvars) (HandleTypeCPS () r2 : params2)
    ProductCPS NotAssignedRgn ts -> do
        ts2 <- mapM (\(a,b)-> (a,) <$> goType r rh b) ts
        return $ ProductCPS (Rgn r) ts2
    ExistsCPS () x body -> do
        body2 <- goType r rh body
        return $ ExistsCPS () x body2
    HandleTypeCPS void _ -> absurd void

goEntry :: KindContextEntry V -> KindContextEntry U
goEntry e = case e of
    TypeEntry id -> TypeEntry id