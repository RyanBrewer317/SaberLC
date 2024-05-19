{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}
module Typechecker (go) where

import Header
import qualified Data.Map as Map

go :: ExprParse -> SLC ExprTC
go = goExpr Map.empty

goExpr :: Map.Map String (Int, TypeTC) -> ExprParse -> SLC ExprTC
goExpr renames e = case e of
    VarParse isGlobal name ->
        case Map.lookup name renames of
            Just (idNum, t) -> return $ VarTC t isGlobal $ Local idNum name
            Nothing -> throw $ UnknownIdentifier name
    IntLitParse i -> return $ IntLitTC i
    LambdaParse param paramType body -> do
        paramIdNum <- fresh
        paramTypeTC <- goType renames paramType
        bodyTC <- goExpr (Map.insert param (paramIdNum, paramTypeTC) renames) body
        return $ LambdaTC paramIdNum param paramTypeTC bodyTC
    AppParse func arg -> do
        funcTC <- goExpr renames func
        argTC <- goExpr renames arg
        case typeOfTC funcTC of
            FunctionTypeTC paramType bodyType ->
                if eqTC (typeOfTC argTC) paramType then
                    return $ AppTC bodyType funcTC argTC
                else
                    throw $ TypeError paramType (typeOfTC argTC)
            t -> throw $ CallingNonFunction t
    TypeLambdaParse param body -> do
        paramIdNum <- fresh
        bodyTC <- goExpr (Map.insert param (paramIdNum, undefined) renames) body
        return $ TypeLambdaTC paramIdNum param bodyTC
    TypeAppParse func arg -> do
        funcTC <- goExpr renames func
        argTC <- goType renames arg
        case typeOfTC funcTC of
            ForallTC paramIdNum _ bodyTC ->
                return $ TypeAppTC (substitute paramIdNum argTC bodyTC) funcTC argTC
            t -> throw $ CallingNonForall t

goType :: Map.Map String (Int, TypeTC) -> TypeParse -> SLC TypeTC
goType renames t = case t of
    TypeVarParse name ->
        case Map.lookup name renames of
            Just (idNum, _) -> return $ TypeVarTC $ Local idNum name
            Nothing -> throw $ UnknownIdentifier name
    I32Parse -> return I32TC
    ForallParse param body -> do
        paramIdNum <- fresh
        bodyTC <- goType (Map.insert param (paramIdNum, undefined) renames) body
        return $ ForallTC paramIdNum param bodyTC
    FunctionTypeParse paramType resultType -> do
        paramTypeTC <- goType renames paramType
        resultTypeTC <- goType renames resultType
        return $ FunctionTypeTC paramTypeTC resultTypeTC