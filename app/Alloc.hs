{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE GADTs #-}

module Alloc where

import Header
import Data.Bifunctor (second)
import Control.Monad (mapAndUnzipM)
import Data.Void (absurd)
import Prelude hiding (id)

data Decl = Malloc Id [TypeCPS U U U V] | Init Id (ValCPS U U U V) Int (ValCPS U U U V)

go :: [Stmt V V] -> SLC [Stmt U V]
go = mapM goStmt

goStmt :: Stmt V V -> SLC (Stmt U V)
goStmt (Func id tvars params body) = Func id tvars (map (second goType) params) <$> goExpr body

goExpr :: ExprCPS U U V V -> SLC (ExprCPS U U U V)
goExpr e = case e of
    AppCPS f ts as -> do
        (f2, decls) <- goVal f
        (as2, decls2) <- mapAndUnzipM goVal as
        return $ putDecls (decls ++ concat decls2) $ AppCPS f2 (map goType ts) as2
    HaltCPS v -> do
        (v2, decls) <- goVal v
        return $ putDecls decls $ HaltCPS v2
    LetCPS x t v scope -> do
        (v2, decls) <- goVal v
        scope2 <- goExpr scope
        let t2 = goType t
        return $ putDecls decls $ LetCPS x t2 v2 scope2
    TupleProjCPS x tpl i scope -> do
        (tpl2, decls) <- goVal tpl
        scope2 <- goExpr scope
        return $ putDecls decls $ TupleProjCPS x tpl2 i scope2
    UnpackCPS () t x v scope -> do
        (v2, decls) <- goVal v
        scope2 <- goExpr scope
        return $ putDecls decls $ UnpackCPS () t x v2 scope2
    MallocCPS v _ _ _ _ -> absurd v
    InitCPS v _ _ _ _ _ -> absurd v

putDecls :: [Decl] -> ExprCPS U U U V -> ExprCPS U U U V
putDecls decls e = foldr (\d e2 ->
        case d of
            Malloc x ts -> MallocCPS () x ts NotAssignedRgn e2
            Init x tpl i v2 -> InitCPS () x tpl i v2 e2
        ) e decls

goVal :: ValCPS U U V V -> SLC (ValCPS U U U V, [Decl])
goVal v = case v of
    LitCPS lit -> return (LitCPS lit, [])
    VarCPS x t global -> return (VarCPS x (goType t) global, [])
    LambdaCPS (NotTrue void) _ _ _ -> absurd void
    TupleCPS NotAssignedRgn NotFalse vs -> do
        (vs2, decls) <- mapAndUnzipM (goVal.snd) vs
        x <- fresh
        let xt = goType $ getValCPSType v
        let first_var = VarCPS x xt False
        (end, decls2) <- getDecls vs2 0 first_var
        return (end, concat decls++Malloc x (map getValCPSType vs2):decls2)
    TAppCPS () t v2 ts -> do
        (v3, decls) <- goVal v2
        return (TAppCPS () (goType t) v3 (map goType ts), decls)
    PackCPS () t v2 t2 -> do
        (v3, decls) <- goVal v2
        return (PackCPS () (goType t) v3 (goType t2), decls)

getDecls :: [ValCPS U U U V] -> Int -> ValCPS U U U V -> SLC (ValCPS U U U V, [Decl])
getDecls [] _ v = return (v, [])
getDecls (v:vs) i last_var = do
    x <- fresh
    let xt = getValCPSType v
    let decl = Init x last_var i v
    (end, rest) <- getDecls vs (i+1) (VarCPS x xt False)
    return (end, decl:rest)

goType :: TypeCPS U U V V -> TypeCPS U U U V
goType t = case t of
    I32CPS -> I32CPS
    TVarCPS tv -> TVarCPS tv
    ArrowCPS xs ts -> ArrowCPS xs (map goType ts)
    ProductCPS NotAssignedRgn xs -> ProductCPS NotAssignedRgn (map (\(_,t2)->(Initialized, goType t2)) xs)
    ExistsCPS () x t2 -> ExistsCPS () x (goType t2)
    HandleTypeCPS v _ -> absurd v