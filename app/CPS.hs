{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module CPS where
import Header
import Prelude hiding (id)

go :: Expr -> SLC (ExprCPS V V V)
go e = do
    x <- fresh
    let t = goType $ getType e
    goExpr e (LambdaCPS NotFalse [] [(x, t)] $ HaltCPS $ VarCPS x t)

goExpr :: Expr -> ValCPS V V V -> SLC (ExprCPS V V V)
goExpr e k = case e of
    Var x t -> return $ AppCPS k [] [VarCPS x $ goType t]
    Lit i -> return $ AppCPS k [] [LitCPS i]
    Lambda x t body -> do
        cont <- fresh
        let cont_t = ArrowCPS [] [goType $ getType body]
        body2 <- goExpr body $ VarCPS cont cont_t
        return $ AppCPS k [] [LambdaCPS NotFalse [] [(x, goType t), (cont, cont_t)] body2]
    App _t e1 e2 -> do
        x1 <- fresh
        x2 <- fresh
        cont <- goExpr e2 $ LambdaCPS NotFalse [] [(x2, goType $ getType e2)] $
                    AppCPS (VarCPS x1 $ goType $ getType e1) [] [VarCPS x2 $ goType $ getType e2, k]
        goExpr e1 $ LambdaCPS NotFalse [] [(x1, goType $ getType e1)] cont
    TLambda x body -> do
        cont <- fresh
        let cont_t = ArrowCPS [] [goType $ getType body]
        body2 <- goExpr body $ VarCPS cont cont_t
        return $ AppCPS k [] [LambdaCPS NotFalse [x] [(cont, cont_t)] body2]
    TApp _t e1 targ -> do
        x <- fresh
        let xt = goType $ getType e1
        goExpr e1 $ LambdaCPS NotFalse [] [(x, xt)] $ AppCPS (VarCPS x xt) [goType targ] [k]

goType :: Type -> TypeCPS V V V
goType t = case t of
    I32 -> I32CPS
    TVar id -> TVarCPS id
    Forall id body -> ArrowCPS [id] [ArrowCPS [] [goType body]]
    Arrow t1 t2 -> ArrowCPS [] [goType t1, ArrowCPS [] [goType t2]]