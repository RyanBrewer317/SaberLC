{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE GADTs #-}

module Hoist(go) where
import Header
import Control.Monad (mapAndUnzipM, (>=>))
import Data.Bifunctor (bimap, first, second)
import Data.Void (absurd)

go :: ExprCPS U V V -> SLC [Stmt V]
go = goExpr >=> \(e, stmts) -> return $ Func (-1) [] [] e : stmts

goExpr :: ExprCPS U V V -> SLC (ExprCPS U U V, [Stmt V])
goExpr e = case e of
    AppCPS f ts as -> goVal f >>= \(f2,stmts)-> bimap (AppCPS f2 (map goType ts)) ((stmts++) . concat) <$> mapAndUnzipM goVal as
    HaltCPS v -> first HaltCPS <$> goVal v
    LetCPS x t v scope -> do
        (v2, stmts) <- goVal v
        (scope2, stmts2) <- goExpr scope
        return (LetCPS x (goType t) v2 scope2, stmts ++ stmts2)
    TupleProjCPS x tpl i scope -> do
        (v2, stmts) <- goVal tpl
        (scope2, stmst2) <- goExpr scope
        return (TupleProjCPS x v2 i scope2, stmts ++ stmst2)
    UnpackCPS () x y v scope -> do
        (v2, stmts) <- goVal v
        (scope2, stmts2) <- goExpr scope
        return (UnpackCPS () x y v2 scope2, stmts ++ stmts2)
    MallocCPS v _ _ _  -> absurd v
    InitCPS v _ _ _ _ _ -> absurd v

goVal :: ValCPS U V V -> SLC (ValCPS U U V, [Stmt V])
goVal v = case v of
    LitCPS i -> return (LitCPS i, [])
    VarCPS x t global -> return (VarCPS x (goType t) global, [])
    f@(LambdaCPS _ tvars xs e) -> do
        x <- fresh
        (e2, stmts) <- goExpr e
        return (VarCPS x (goType $ getValCPSType f) True, Func x tvars (map (second goType) xs) e2 : stmts)
    TupleCPS n xs -> bimap (TupleCPS n) concat <$> mapAndUnzipM (\(PreAlloc,x)->goVal x >>= \(x2,stmts)->return((PreAlloc,x2),stmts)) xs
    TAppCPS () t e ts -> goVal e >>= \(e2,stmts)-> return (TAppCPS () (goType t) e2 (map goType ts), stmts)
    PackCPS () t v2 t2 -> goVal v2 >>= \(v3,stmts)-> return (PackCPS () (goType t) v3 (goType t2), stmts)

goType :: TypeCPS U V V -> TypeCPS U U V
goType t = case t of
    I32CPS -> I32CPS
    TVarCPS x -> TVarCPS x
    ArrowCPS tvars xs -> ArrowCPS tvars (map goType xs)
    ProductCPS ts -> ProductCPS (map (second goType) ts)
    ExistsCPS () x t2 -> ExistsCPS () x (goType t2)