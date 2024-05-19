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
    e2 <- goExpr e (LambdaCPS [(x, "B", t)] $ HaltCPS $ VarCPS t False $ Local x "B")
    typecheckExpr e2
    return e2

goExpr :: ExprTC -> ValCPS -> SLC ExprCPS
goExpr e k = case e of
    VarTC t isGlobal ident -> return $ AppCPS k [VarCPS (goType t) isGlobal ident]
    IntLitTC i -> return $ AppCPS k [IntLitCPS i]
    LambdaTC idNum s argt body -> do
        continuationIdNum <- fresh
        let continuationType = FunctionTypeCPS [goType $ typeOfTC body]
        body2 <- goExpr body $ VarCPS continuationType False $ Local continuationIdNum "k"
        return $ AppCPS k [LambdaCPS [(idNum, s, goType argt), (continuationIdNum, "k", continuationType)] body2]
    AppTC _t f arg -> do
        x1 <- fresh
        x2 <- fresh
        continuation <-
            goExpr arg $
                LambdaCPS [(x2, "BLA", goType $ typeOfTC arg)] $
                    AppCPS (VarCPS (goType $ typeOfTC f) False $ Local x1 "BL") [VarCPS (goType $ typeOfTC arg) False $ Local x2 "BLA", k]
        goExpr f $ LambdaCPS [(x1, "BL", goType $ typeOfTC f)] continuation
    TypeLambdaTC idNum s body -> do
        continuationIdNum <- fresh
        let continuationType = FunctionTypeCPS [goType $ typeOfTC body]
        body2 <- goExpr body $ VarCPS continuationType False $ Local continuationIdNum "k"
        return $ AppCPS k [TypeLambdaCPS idNum s $ LambdaCPS [(continuationIdNum, "k", continuationType)] body2]
    TypeAppTC t f typeArg -> do
        x <- fresh
        let xt = goType $ typeOfTC f
        goExpr f $ LambdaCPS [(x, "BLAH", xt)] $ AppCPS (TypeAppCPS (goType t) (VarCPS xt False $ Local x "BLAH") $ goType typeArg) [k]

goType :: TypeTC -> TypeCPS
goType t = case t of
    TypeVarTC ident -> TypeVarCPS ident
    I32TC -> I32CPS
    ForallTC idNum s body -> ForallCPS idNum s $ goType body
    FunctionTypeTC argt rett -> FunctionTypeCPS [goType argt, FunctionTypeCPS [goType rett]]

subst :: Int -> TypeCPS -> TypeCPS -> TypeCPS
subst idNum newt oldt = case oldt of
    TypeVarCPS (Local i _) | i == idNum -> newt
    TypeVarCPS _ -> oldt
    I32CPS -> oldt
    ForallCPS idNum1 s body -> ForallCPS idNum1 s (subst idNum newt body)
    FunctionTypeCPS argTypes -> FunctionTypeCPS (map (subst idNum newt) argTypes)

typeEq :: TypeCPS -> TypeCPS -> Bool
typeEq t1 t2 = case (t1, t2) of
    (TypeVarCPS i1, TypeVarCPS i2) -> i1 == i2
    (I32CPS, I32CPS) -> True
    (ForallCPS idNum1 s1 body1, ForallCPS idNum2 _ body2) -> typeEq body1 (subst idNum2 (TypeVarCPS $ Local idNum1 s1) body2)
    (FunctionTypeCPS argTypes1, FunctionTypeCPS argTypes2) -> all (uncurry typeEq) $ zip argTypes1 argTypes2
    _ -> False

typecheckExpr :: ExprCPS -> SLC ()
typecheckExpr e = case e of
    HaltCPS v -> do
        vt <- typecheckVal v
        if vt == I32CPS then
            return ()
        else
            error "Type mismatch in halt expression in CPS pass"
    AppCPS f as -> do
        ft <- typecheckVal f
        case ft of
            FunctionTypeCPS argTypes ->
                if length argTypes == length as && all (uncurry typeEq) (zip argTypes (map typeOfCPSVal as)) then
                    return ()
                else
                    error "Type mismatch in function application in CPS pass"
            _ -> error "app of nonfunction in CPS pass"

typecheckVal :: ValCPS -> SLC TypeCPS
typecheckVal v = case v of
    VarCPS t _ _ -> return t
    IntLitCPS _ -> return I32CPS
    LambdaCPS params body -> do
        typecheckExpr body
        return $ FunctionTypeCPS $ map (\(_,_,t)->t) params
    TypeLambdaCPS idNum s body -> do
        bodyt <- typecheckVal body
        return $ ForallCPS idNum s bodyt
    TypeAppCPS t f typeArg -> do
        ft <- typecheckVal f
        case ft of
            ForallCPS idNum _ body ->
                if typeEq t (subst idNum typeArg body) then
                    return t
                else
                    error "Type mismatch in type application in CPS pass"
            _ -> error "Non-forall applied to type in CPS pass"