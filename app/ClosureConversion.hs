{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module ClosureConversion where

import Header

go :: ExprCPS -> SLC ExprCC
go = goExpr

goExpr :: ExprCPS -> SLC ExprCC
goExpr e = case e of
    HaltCPS v -> HaltCC <$> goVal v
    AppCPS f as -> do
        gamma <- fresh
        z <- fresh
        zcode <- fresh
        zenv <- fresh
        f2 <- goVal f
        let (ExistialCC _id (TupleTypeCC [tcode, TypeVarCC _])) = typeOfCCVal f2
        let prodt = TupleTypeCC [tcode, TypeVarCC $ Local gamma]
        as2 <- mapM goVal as
        return $
            UnpackCC gamma z f2 $
                ProjCC zcode tcode (VarCC prodt False (Local z)) 0 $
                    ProjCC zenv (TypeVarCC $ Local gamma) (VarCC prodt False (Local z)) 1 $
                        AppCC (VarCC tcode False (Local zcode)) (VarCC (TypeVarCC (Local gamma)) False (Local zenv) : as2)

goVal :: ValCPS -> SLC ValCC
goVal v = case v of
    VarCPS t isGlobal ident -> goType t >>= \t2 -> return $ VarCC t2 isGlobal ident
    IntLitCPS i -> return $  IntLitCC i
    LambdaCPS params body -> do
        let functionFreeVars = fvVal [] v
        functionFreeVars2 <- mapM (\(x, t) -> goType t >>= \t2 -> return (x, t2)) functionFreeVars
        let functionFreeTypeVars = ftvVal [] v
        let envType = TupleTypeCC $ map snd functionFreeVars2
        codeType <- FunctionTypeCC . (envType :) <$> mapM (goType . snd) params
        envIdNum <- fresh
        body2 <- goExpr body
        args2 <- ((envIdNum, envType) :) <$> mapM (\(x, t) -> goType t >>= \t2 -> return (x, t2)) params
        let code = LambdaCC args2 $ foldr (\(i, (idNum, t)) e -> 
                    ProjCC idNum t (VarCC envType False $ Local envIdNum) i e
                ) body2 $ zip [0 ..] functionFreeVars2
        let envVal = TupleCC $ map (\(x, t) -> VarCC t False $ Local x) functionFreeVars2
        functionType <- goType $ typeOfCPSVal v
        let codeWithTypes = if null functionFreeTypeVars then code else TypeAppCC codeType code (map (TypeVarCC . Local) functionFreeTypeVars)
        return $ PackCC envType (TupleCC [codeWithTypes, envVal]) functionType
    TypeLambdaCPS params body -> TypeLambdaCC params <$> goVal body
    TypeAppCPS t f typeArgs -> TypeAppCC <$> goType t <*> goVal f <*> mapM goType typeArgs

goType :: TypeCPS -> SLC TypeCC
goType t = case t of
    TypeVarCPS ident -> return $ TypeVarCC ident
    I32CPS -> return I32CC
    ForallCPS param body -> ForallCC [param] <$> goType body
    FunctionTypeCPS params -> do
        x <- fresh
        params2 <- mapM goType params
        return $ ExistialCC x (TupleTypeCC [FunctionTypeCC (TypeVarCC (Local x) : params2), TypeVarCC (Local x)])

fv :: [Int] -> ExprCPS -> [(Int, TypeCPS)]
fv bound e = case e of
    HaltCPS v -> fvVal bound v
    AppCPS f as -> fvVal bound f ++ concatMap (fvVal bound) as

fvVal :: [Int] -> ValCPS -> [(Int, TypeCPS)]
fvVal bound v = case v of
    VarCPS t _ (Local idNum) -> ([(idNum, t) | idNum `notElem` bound])
    VarCPS {} -> []
    IntLitCPS _ -> []
    LambdaCPS args body -> fv (map fst args ++ bound) body
    TypeLambdaCPS _ body -> fvVal bound body
    TypeAppCPS _ f _ -> fvVal bound f

ftv :: [Int] -> ExprCPS -> [Int]
ftv bound e = case e of
    HaltCPS v -> ftvVal bound v
    AppCPS f as -> ftvVal bound f ++ concatMap (ftvVal bound) as

ftvVal :: [Int] -> ValCPS -> [Int]
ftvVal bound v = case v of
    VarCPS t _ _ -> ftvType bound t
    IntLitCPS _ -> []
    LambdaCPS args body -> concatMap (ftvType bound . snd) args ++ ftv bound body
    TypeLambdaCPS params body -> ftvVal (params ++ bound) body
    TypeAppCPS t f ts -> ftvType bound t ++ ftvVal bound f ++ concatMap (ftvType bound) ts

ftvType :: [Int] -> TypeCPS -> [Int]
ftvType bound t = case t of
    TypeVarCPS (Local idNum) -> ([idNum | idNum `notElem` bound])
    TypeVarCPS {} -> []
    I32CPS -> []
    ForallCPS idNum body -> ftvType (idNum : bound) body
    FunctionTypeCPS params -> concatMap (ftvType bound) params

typeOfCCVal :: ValCC -> TypeCC
typeOfCCVal v = case v of
    VarCC t _ _ -> t
    IntLitCC _ -> I32CC
    LambdaCC args _ -> FunctionTypeCC $ map snd args
    PackCC _t _v t2 -> t2 -- TODO
    TypeLambdaCC params body -> ForallCC params $ typeOfCCVal body
    TypeAppCC t _f _ts -> t
    TupleCC vs -> TupleTypeCC $ map typeOfCCVal vs