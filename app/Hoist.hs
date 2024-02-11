{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Hoist(go) where
import Header
import Control.Monad (mapAndUnzipM, (>=>))
import Data.Bifunctor (bimap, first)

go :: ExprCPS () -> SLC [Stmt]
go = goExpr >=> \(e, stmts) -> return $ Func (-1) [] [] e : stmts

goExpr :: ExprCPS () -> SLC (ExprCPS (), [Stmt])
goExpr e = case e of
    AppCPS f ts as -> goVal f >>= \(f2,stmts)-> bimap (AppCPS f2 ts) ((stmts++) . concat) <$> mapAndUnzipM goVal as
    HaltCPS v -> first HaltCPS <$> goVal v
    LetCPS x t v scope -> do
        (v2, stmts) <- goVal v
        (scope2, stmts2) <- goExpr scope
        return (LetCPS x t v2 scope2, stmts ++ stmts2)
    TupleProjCPS x tpl i scope -> do
        (v2, stmts) <- goVal tpl
        (scope2, stmst2) <- goExpr scope
        return (TupleProjCPS x v2 i scope2, stmts ++ stmst2)
    UnpackCPS x y v scope () -> do
        (v2, stmts) <- goVal v
        (scope2, stmts2) <- goExpr scope
        return (UnpackCPS x y v2 scope2 (), stmts ++ stmts2)

goVal :: ValCPS () -> SLC (ValCPS (), [Stmt])
goVal v = case v of
    LitCPS _ -> return (v, [])
    VarCPS _ _ -> return (v, [])
    f@(LambdaCPS tvars xs e) -> do
        x <- fresh
        (e2, stmts) <- goExpr e
        return (VarCPS x (getValCPSType f), Func x tvars xs e2 : stmts)
    TupleCPS xs -> bimap TupleCPS concat <$> mapAndUnzipM goVal xs
    TAppCPS t e ts () -> goVal e >>= \(e2,stmts)-> return (TAppCPS t e2 ts (), stmts)
    PackCPS t v2 t2 () -> goVal v2 >>= \(v3,stmts)-> return (PackCPS t v3 t2 (), stmts)