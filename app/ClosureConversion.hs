{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module ClosureConversion (go) where

import Header

go :: ExprCPS -> SLC ExprCC
go = goExpr

goExpr :: ExprCPS -> SLC ExprCC
goExpr e = case e of
    HaltCPS v -> HaltCC <$> goVal v
    AppCPS f as -> do
        tpl <- fresh
        new_f <- fresh
        env <- fresh
        clos <- goVal f
        let (ExistialCC hidden_t (TupleTypeCC [new_f_t, TypeVarCC _])) = typeOfCCVal clos
        let prodt = TupleTypeCC [new_f_t, TypeVarCC $ Local hidden_t]
        as2 <- mapM goVal as
        return $
            UnpackCC hidden_t tpl clos $
                ProjCC new_f new_f_t (VarCC prodt False (Local tpl)) 0 $
                    ProjCC env (TypeVarCC $ Local hidden_t) (VarCC prodt False (Local tpl)) 1 $
                        AppCC (VarCC new_f_t False (Local new_f)) (VarCC (TypeVarCC (Local hidden_t)) False (Local env) : as2)

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
    TypeLambdaCPS param body -> TypeLambdaCC [param] <$> goVal body
    TypeAppCPS t f typeArgs -> TypeAppCC <$> goType t <*> goVal f <*> ((:[]) <$> goType typeArgs)

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
    TypeLambdaCPS param body -> ftvVal (param : bound) body
    TypeAppCPS t f arg -> ftvType bound t ++ ftvVal bound f ++ ftvType bound arg

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