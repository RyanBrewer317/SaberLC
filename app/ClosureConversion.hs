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
        let (ExistialCC hidden_t "a" (TupleTypeCC [new_f_t, TypeVarCC _])) = typeOfCCVal clos
        let prodt = TupleTypeCC [new_f_t, TypeVarCC $ Local hidden_t "a"]
        as2 <- mapM goVal as
        return $
            UnpackCC hidden_t "a" tpl "clos" clos $
                ProjCC new_f "f" new_f_t (VarCC prodt False (Local tpl "f")) 0 $
                    ProjCC env "env" (TypeVarCC $ Local hidden_t "a") (VarCC prodt False (Local tpl "clos")) 1 $
                        AppCC (VarCC new_f_t False (Local new_f "f")) (VarCC (TypeVarCC (Local hidden_t "a")) False (Local env "env") : as2)

goVal :: ValCPS -> SLC ValCC
goVal v = case v of
    VarCPS t isGlobal ident -> goType t >>= \t2 -> return $ VarCC t2 isGlobal ident
    IntLitCPS i -> return $  IntLitCC i
    LambdaCPS params body -> do
        let functionFreeVars = fvVal [] v
        functionFreeVars2 <- mapM (\(x, s, t) -> goType t >>= \t2 -> return (x, s, t2)) functionFreeVars
        let functionFreeTypeVars = ftvVal [] v
        let envType = TupleTypeCC $ map (\(_, _, t)->t) functionFreeVars2
        codeType <- FunctionTypeCC . (envType :) <$> mapM (\(_, _, t)->goType t) params
        envIdNum <- fresh
        body2 <- goExpr body
        args2 <- ((envIdNum, "env", envType) :) <$> mapM (\(x, s, t) -> goType t >>= \t2 -> return (x, s, t2)) params
        let code = LambdaCC args2 $ foldr (\(i, (idNum, s, t)) e ->
                    ProjCC idNum s t (VarCC envType False $ Local envIdNum s) i e
                ) body2 $ zip [0 ..] functionFreeVars2
        let envVal = TupleCC $ map (\(x, s, t) -> VarCC t False $ Local x s) functionFreeVars2
        functionType <- goType $ typeOfCPSVal v
        let codeWithTypes = if null functionFreeTypeVars then code else TypeAppCC codeType code (map (\(idNum,s)->TypeVarCC $ Local idNum s) functionFreeTypeVars)
        return $ PackCC envType (TupleCC [codeWithTypes, envVal]) functionType
    TypeLambdaCPS param s body -> TypeLambdaCC [(param, s)] <$> goVal body
    TypeAppCPS t f typeArgs -> TypeAppCC <$> goType t <*> goVal f <*> ((:[]) <$> goType typeArgs)

goType :: TypeCPS -> SLC TypeCC
goType t = case t of
    TypeVarCPS ident -> return $ TypeVarCC ident
    I32CPS -> return I32CC
    ForallCPS param s body -> ForallCC [(param, s)] <$> goType body
    FunctionTypeCPS params -> do
        x <- fresh
        params2 <- mapM goType params
        return $ ExistialCC x "a" (TupleTypeCC [FunctionTypeCC (TypeVarCC (Local x "a") : params2), TypeVarCC (Local x "a")])

fv :: [Int] -> ExprCPS -> [(Int, String, TypeCPS)]
fv bound e = case e of
    HaltCPS v -> fvVal bound v
    AppCPS f as -> fvVal bound f ++ concatMap (fvVal bound) as

fvVal :: [Int] -> ValCPS -> [(Int, String, TypeCPS)]
fvVal bound v = case v of
    VarCPS t _ (Local idNum s) -> ([(idNum, s, t) | idNum `notElem` bound])
    VarCPS {} -> []
    IntLitCPS _ -> []
    LambdaCPS args body -> fv (map (\(idNum,_,_)->idNum) args ++ bound) body
    TypeLambdaCPS _ _ body -> fvVal bound body
    TypeAppCPS _ f _ -> fvVal bound f

ftv :: [Int] -> ExprCPS -> [(Int, String)]
ftv bound e = case e of
    HaltCPS v -> ftvVal bound v
    AppCPS f as -> ftvVal bound f ++ concatMap (ftvVal bound) as

ftvVal :: [Int] -> ValCPS -> [(Int, String)]
ftvVal bound v = case v of
    VarCPS t _ _ -> ftvType bound t
    IntLitCPS _ -> []
    LambdaCPS args body -> concatMap (\(_,_,t)->ftvType bound t) args ++ ftv bound body
    TypeLambdaCPS param _ body -> ftvVal (param : bound) body
    TypeAppCPS t f arg -> ftvType bound t ++ ftvVal bound f ++ ftvType bound arg

ftvType :: [Int] -> TypeCPS -> [(Int, String)]
ftvType bound t = case t of
    TypeVarCPS (Local idNum s) -> ([(idNum, s) | idNum `notElem` bound])
    TypeVarCPS {} -> []
    I32CPS -> []
    ForallCPS idNum _ body -> ftvType (idNum : bound) body
    FunctionTypeCPS params -> concatMap (ftvType bound) params

typeOfCCVal :: ValCC -> TypeCC
typeOfCCVal v = case v of
    VarCC t _ _ -> t
    IntLitCC _ -> I32CC
    LambdaCC args _ -> FunctionTypeCC $ map (\(_,_,t)->t) args
    PackCC _t _v t2 -> t2 -- TODO
    TypeLambdaCC params body -> ForallCC params $ typeOfCCVal body
    TypeAppCC t _f _ts -> t
    TupleCC vs -> TupleTypeCC $ map typeOfCCVal vs