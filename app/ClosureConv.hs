{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ClosureConv (go) where

import Control.Monad ((>=>))
import Data.Void (absurd)
import Header
import Prelude hiding (id)

go :: ExprCPS V V V V -> SLC (ExprCPS U V V V)
go e = case e of
  AppCPS f ts as -> do
    gamma <- fresh
    z <- fresh
    zcode <- fresh
    zenv <- fresh
    f2 <- goVal f
    let (ExistsCPS () _id (ProductCPS NotAssignedRgn [(PreAlloc, tcode), (PreAlloc, TVarCPS _)])) = getValCPSType f2
    let prodt = ProductCPS NotAssignedRgn [(PreAlloc, tcode), (PreAlloc, TVarCPS gamma)]
    as2 <- mapM goVal as
    ts2 <- mapM goCTArg ts
    return $
      UnpackCPS () gamma z f2 $
        TupleProjCPS zcode (VarCPS z prodt False) 0 $
          TupleProjCPS zenv (VarCPS z prodt False) 1 $
            AppCPS (VarCPS zcode tcode False) ts2 (VarCPS zenv (TVarCPS gamma) False : as2)
  HaltCPS v -> HaltCPS <$> goVal v
  LetCPS id t v scope -> LetCPS id <$> goType t <*> goVal v <*> go scope
  TupleProjCPS id tpl i scope -> TupleProjCPS id <$> goVal tpl <*> pure i <*> go scope
  UnpackCPS void _ _ _ _ -> absurd void
  MallocCPS void _ _ _ _ _ -> absurd void
  InitCPS void _ _ _ _ _ -> absurd void

goVal :: ValCPS V V V V -> SLC (ValCPS U V V V)
goVal v = case v of
  LitCPS l -> return $ LitCPS l
  VarCPS id t global -> flip (VarCPS id) global <$> goType t
  f@(LambdaCPS _ tvars NotAssignedCap args body) -> do
    let ys = fvVal [] f
    let ts = ftvVal [] f
    t_env <- goType $ ProductCPS NotAssignedRgn $ map (\(_, t, _) -> (PreAlloc, t)) ys
    -- let t_rawcode = ArrowCPS (ts++tvars) $ t_env : map (goType . snd) args -- used for recursion, which I don't support yet
    t_code <- ArrowCPS tvars NotAssignedCap . (t_env :) <$> mapM (goType . snd) args
    z_env <- fresh
    body2 <- go body
    args2 <- ((z_env, t_env) :) <$> mapM (\(x, t) -> goType t >>= \t2 -> return (x, t2)) args
    let v_code =
          LambdaCPS NotFalse (ts ++ tvars) NotAssignedCap args2 $
            foldr (\(i, (x, _t, _global)) e -> TupleProjCPS x (VarCPS z_env t_env False) i e) body2 $
              zip [0 ..] ys
    v_env <- TupleCPS NotAssignedRgn NotFalse <$> mapM (\(x, t, global) -> goType t >>= \t2 -> return (PreAlloc, VarCPS x t2 global)) ys
    ft <- goType $ getValCPSType f
    let with_ts = if null ts then v_code else TAppCPS () t_code v_code (map TVarCPS $ ids ts)
    return $ PackCPS () t_env (TupleCPS NotAssignedRgn NotFalse [(PreAlloc, with_ts), (PreAlloc, v_env)]) ft
  TupleCPS NotAssignedRgn n vals -> TupleCPS NotAssignedRgn n <$> mapM (return . snd >=> goVal >=> return . (PreAlloc,)) vals
  TAppCPS void _ _ _ -> absurd void
  PackCPS void _ _ _ -> absurd void

goType :: TypeCPS V V V V -> SLC (TypeCPS U V V V)
goType t = case t of
  I32CPS -> return I32CPS
  TVarCPS id -> return $ TVarCPS id
  ArrowCPS tvars NotAssignedCap args -> do
    x <- fresh
    ts <- mapM goType args
    return $ ExistsCPS () x (ProductCPS NotAssignedRgn [(PreAlloc, ArrowCPS tvars NotAssignedCap (TVarCPS x : ts)), (PreAlloc, TVarCPS x)])
  ProductCPS NotAssignedRgn ts -> ProductCPS NotAssignedRgn <$> mapM (return . snd >=> goType >=> return . (PreAlloc,)) ts
  ExistsCPS void _ _ -> absurd void
  HandleTypeCPS void _ -> absurd void

fv :: [Id] -> ExprCPS V V V V -> [(Id, TypeCPS V V V V, Bool)]
fv bound e = case e of
  AppCPS f _ts as -> fvVal bound f ++ concatMap (fvVal bound) as
  HaltCPS v -> fvVal bound v
  LetCPS id _t v scope -> fvVal bound v ++ fv (id : bound) scope
  TupleProjCPS id tpl _i scope -> fvVal bound tpl ++ fv (id : bound) scope
  UnpackCPS void _ _ _ _ -> absurd void
  MallocCPS void _ _ _ _ _ -> absurd void
  InitCPS void _ _ _ _ _ -> absurd void

fvVal :: [Id] -> ValCPS V V V V -> [(Id, TypeCPS V V V V, Bool)]
fvVal bound v = case v of
  LitCPS _ -> []
  VarCPS id t global -> ([(id, t, global) | id `notElem` bound])
  LambdaCPS _ _tvars NotAssignedCap args body -> fv (map fst args ++ bound) body
  TupleCPS NotAssignedRgn _ vals -> concatMap (fvVal bound . snd) vals
  TAppCPS void _ _ _ -> absurd void
  PackCPS void _ _ _ -> absurd void

ftv :: [KindContextEntry V] -> ExprCPS V V V V -> [KindContextEntry V]
ftv bound e = case e of
  AppCPS f ts as -> ftvVal bound f ++ concatMap (ftvCTArg bound) ts ++ concatMap (ftvVal bound) as
  HaltCPS v -> ftvVal bound v
  LetCPS _id t v scope -> ftvVal bound v ++ ftvType bound t ++ ftv bound scope
  TupleProjCPS _id tpl _i scope -> ftvVal bound tpl ++ ftv bound scope
  UnpackCPS void _ _ _ _ -> absurd void
  MallocCPS void _ _ _ _ _ -> absurd void
  InitCPS void _ _ _ _ _ -> absurd void

ftvVal :: [KindContextEntry V] -> ValCPS V V V V -> [KindContextEntry V]
ftvVal bound v = case v of
  LitCPS _ -> []
  VarCPS _ t _ -> ftvType bound t
  LambdaCPS _ tvars NotAssignedCap args body -> concatMap (ftvType (tvars ++ bound) . snd) args ++ ftv (tvars ++ bound) body
  TupleCPS NotAssignedRgn _ vals -> concatMap (ftvVal bound . snd) vals
  TAppCPS void _ _ _ -> absurd void
  PackCPS void _ _ _ -> absurd void

ftvType :: [KindContextEntry V] -> TypeCPS V V V V -> [KindContextEntry V]
ftvType bound t = case t of
  I32CPS -> []
  TVarCPS id -> ([TypeEntry id | TypeEntry id `notElem` bound])
  ArrowCPS tvars NotAssignedCap args -> concatMap (ftvType (tvars ++ bound)) args
  ProductCPS NotAssignedRgn ts -> concatMap (ftvType bound . snd) ts
  ExistsCPS void _ _ -> absurd void
  HandleTypeCPS void _ -> absurd void

ftvCTArg :: [KindContextEntry V] -> CTArg V V V V -> [KindContextEntry V]
ftvCTArg bound a = case a of
  TypeCTArg t -> ftvType bound t
  RgnCTArg void _ -> absurd void
  CapCTArg void _ -> absurd void

goCTArg :: CTArg V V V V -> SLC (CTArg U V V V)
goCTArg a = case a of
  TypeCTArg t -> TypeCTArg <$> goType t
  RgnCTArg void _ -> absurd void
  CapCTArg void _ -> absurd void