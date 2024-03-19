{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}
module CPS (go) where

import Header

go :: ExprTC -> SLC ExprCPS
go e = do
    x <- fresh
    let t = goType $ typeOfTC e
    goExpr e (LambdaCPS [(x, t)] $ HaltCPS $ VarCPS t False $ Local x)

goExpr :: ExprTC -> ValCPS -> SLC ExprCPS
goExpr e k = case e of
    VarTC t isGlobal ident -> return $ AppCPS k [VarCPS (goType t) isGlobal ident]
    IntLitTC i -> return $ AppCPS k [IntLitCPS i]
    LambdaTC idNum argt body -> do
        continuationIdNum <- fresh
        let continuationType = FunctionTypeCPS [goType $ typeOfTC body]
        body2 <- goExpr body $ VarCPS continuationType False $ Local continuationIdNum
        return $ AppCPS k [LambdaCPS [(idNum, goType argt), (continuationIdNum, continuationType)] body2]
    AppTC _t f arg -> do
        x1 <- fresh 
        x2 <- fresh
        continuation <-
            goExpr arg $
                LambdaCPS [(x2, goType $ typeOfTC arg)] $
                    AppCPS (VarCPS (goType $ typeOfTC f) False $ Local x1) [VarCPS (goType $ typeOfTC arg) False $ Local x2, k]
        goExpr f $ LambdaCPS [(x1, goType $ typeOfTC f)] continuation
    TypeLambdaTC idNum body -> do
        continuationIdNum <- fresh
        let continuationType = FunctionTypeCPS [goType $ typeOfTC body]
        body2 <- goExpr body $ VarCPS continuationType False $ Local continuationIdNum
        return $ AppCPS k [TypeLambdaCPS [idNum] $ LambdaCPS [(continuationIdNum, continuationType)] body2]
    TypeAppTC t f typeArg -> do
        x <- fresh
        let xt = goType $ typeOfTC f
        goExpr f $ LambdaCPS [(x, xt)] $ AppCPS (TypeAppCPS (goType t) (VarCPS xt False $ Local x) [goType typeArg]) [k]

goType :: TypeTC -> TypeCPS
goType t = case t of
    TypeVarTC ident -> TypeVarCPS ident
    I32TC -> I32CPS
    ForallTC idNum body -> ForallCPS idNum $ goType body
    FunctionTypeTC argt rett -> FunctionTypeCPS [goType argt, FunctionTypeCPS [goType rett]]