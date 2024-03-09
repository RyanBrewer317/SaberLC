{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}
module AST (go) where

import Data.Map
import Header
import Prelude hiding (fail, id, lookup)

go :: Map String (Id, Type, Bool) -> Map String Id -> ExprSyntax -> SLC Expr
go gamma delta e = case e of
  LambdaSyntax name t body -> do
    id <- fresh
    t2 <- goType delta t
    body2 <- go (insert name (id, t2, False) gamma) delta body
    return $ Lambda id t2 body2
  TLambdaSyntax name body -> do
    id <- fresh
    body2 <- go gamma (insert name id delta) body
    return $ TLambda id body2
  AppSyntax e1 e2 -> do
    e1' <- go gamma delta e1
    e2' <- go gamma delta e2
    case getType e1' of
      Arrow t1 t2 ->
        if typeEq t1 $ getType e2'
          then
            return $ App t2 e1' e2'
          else
            fail $ TypeMismatch t1 (getType e2')
      _ -> fail $ CallingNonFunction (getType e1')
  TAppSyntax f t -> do
    f2 <- go gamma delta f
    t2 <- goType delta t
    case getType f2 of
      Forall id body -> return $ TApp (substitute id t2 body) f2 t2
      _ -> fail $ CallingNonForall (getType f2)
  VarSyntax name -> do
    case lookup name gamma of
      Just (id, t, global) -> return $ Var id t global
      Nothing -> fail $ UnknownIdentifier name
  LitSyntax lit -> return $ Lit lit

goType :: Map String Id -> TypeSyntax -> SLC Type
goType delta t = case t of
  I32Syntax -> return I32
  TVarSyntax name -> do
    case lookup name delta of
      Just id -> return $ TVar id
      Nothing -> fail $ UnknownIdentifier name
  ForallSyntax name body -> do
    id <- fresh
    body2 <- goType (insert name id delta) body
    return $ Forall id body2
  ArrowSyntax t1 t2 -> do
    t1' <- goType delta t1
    t2' <- goType delta t2
    return $ Arrow t1' t2'