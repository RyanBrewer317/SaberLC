{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
module ClosureConv where

import Header
import Data.Void (absurd)
import Prelude hiding (id)
import Control.Monad ((>=>))

go :: ExprCPS V V V -> SLC (ExprCPS U V V)
go e = case e of
   AppCPS f ts as -> do
      gamma <- fresh
      z <- fresh
      zcode <- fresh
      zenv <- fresh
      f2 <- goVal f
      let (ExistsCPS () _id (ProductCPS [(PreAlloc, tcode), (PreAlloc, TVarCPS _)])) = getValCPSType f2
      let prodt = ProductCPS [(PreAlloc,tcode), (PreAlloc,TVarCPS gamma)]
      as2 <- mapM goVal as
      ts2 <- mapM goType ts
      return $ UnpackCPS () gamma z f2 $
            TupleProjCPS zcode (VarCPS z prodt) 0 $
               TupleProjCPS zenv (VarCPS z prodt) 1 $
                  AppCPS (VarCPS zcode tcode) ts2 (VarCPS zenv (TVarCPS gamma) : as2)
   HaltCPS v -> HaltCPS <$> goVal v
   LetCPS id t v scope -> LetCPS id <$> goType t <*> goVal v <*> go scope
   TupleProjCPS id tpl i scope -> TupleProjCPS id <$> goVal tpl <*> pure i <*> go scope
   UnpackCPS void _ _ _ _  -> absurd void
   MallocCPS void _ _ _ -> absurd void
   InitCPS void _ _ _ _ _ -> absurd void

goVal :: ValCPS V V V -> SLC (ValCPS U V V)
goVal v = case v of
   LitCPS l -> return $ LitCPS l
   VarCPS id t -> VarCPS id <$> goType t
   f@(LambdaCPS _ tvars args body) -> do
      let ys = fvVal [] f
      let ts = ftvVal [] f
      t_env <- goType $ ProductCPS $ map ((PreAlloc,).snd) ys
      -- let t_rawcode = ArrowCPS (ts++tvars) $ t_env : map (goType . snd) args -- used for recursion, which I don't support yet
      t_code <- ArrowCPS tvars <$> ((t_env :) <$> mapM (goType . snd) args)
      z_env <- fresh
      body2 <- go body
      args2 <- ((z_env, t_env) :) <$> mapM (\(x,t)-> goType t >>= \t2->return (x,t2)) args
      let v_code = LambdaCPS NotFalse (ts++tvars) args2 $
            foldr (\(i,(x,_t)) e->TupleProjCPS x (VarCPS z_env t_env) i e) body2 $ zip [0..] ys
      v_env <- TupleCPS NotFalse <$> mapM (\(x,t)->goType t >>= \t2->return (PreAlloc,VarCPS x t2)) ys
      ft <- goType $ getValCPSType f
      let with_ts = if null ts then v_code else TAppCPS () t_code v_code (map TVarCPS ts)
      return $ PackCPS () t_env (TupleCPS NotFalse [(PreAlloc,with_ts), (PreAlloc, v_env)]) ft
   TupleCPS n vals -> TupleCPS n <$> mapM (return.snd >=> goVal >=> return.(PreAlloc,)) vals
   TAppCPS void _ _ _ -> absurd void
   PackCPS void _ _ _ -> absurd void

goType :: TypeCPS V V V -> SLC (TypeCPS U V V)
goType t = case t of
   I32CPS -> return I32CPS
   TVarCPS id -> return $ TVarCPS id
   ArrowCPS tvars args -> do
      x <- fresh
      ts <- mapM goType args
      return $ ExistsCPS () x (ProductCPS [(PreAlloc,ArrowCPS tvars (TVarCPS x : ts)), (PreAlloc,TVarCPS x)])
   ProductCPS ts -> ProductCPS <$> mapM (return.snd >=> goType >=> return.(PreAlloc,)) ts
   ExistsCPS void _ _ -> absurd void

fv :: [Id] -> ExprCPS V V V -> [(Id, TypeCPS V V V)]
fv bound e = case e of
   AppCPS f _ts as -> fvVal bound f ++ concatMap (fvVal bound) as
   HaltCPS v -> fvVal bound v
   LetCPS id _t v scope -> fvVal bound v ++ fv (id : bound) scope
   TupleProjCPS id tpl _i scope -> fvVal bound tpl ++ fv (id : bound) scope
   UnpackCPS void _ _ _ _ -> absurd void
   MallocCPS void _ _ _ -> absurd void
   InitCPS void _ _ _ _ _ -> absurd void

fvVal :: [Id] -> ValCPS V V V -> [(Id, TypeCPS V V V)]
fvVal bound v = case v of
   LitCPS _ -> []
   VarCPS id t -> ([(id, t) | id `notElem` bound])
   LambdaCPS _ _tvars args body -> fv (map fst args ++ bound) body
   TupleCPS _ vals -> concatMap (fvVal bound . snd) vals
   TAppCPS void _ _ _ -> absurd void
   PackCPS void _ _ _ -> absurd void

ftv :: [Id] -> ExprCPS V V V -> [Id]
ftv bound e = case e of
   AppCPS f ts as -> ftvVal bound f ++ concatMap (ftvType bound) ts ++ concatMap (ftvVal bound) as
   HaltCPS v -> ftvVal bound v
   LetCPS _id t v scope -> ftvVal bound v ++ ftvType bound t ++ ftv bound scope
   TupleProjCPS _id tpl _i scope -> ftvVal bound tpl ++ ftv bound scope
   UnpackCPS void _ _ _ _ -> absurd void
   MallocCPS void _ _ _ -> absurd void
   InitCPS void _ _ _ _ _ -> absurd void

ftvVal :: [Id] -> ValCPS V V V -> [Id]
ftvVal bound v = case v of
   LitCPS _ -> []
   VarCPS _ t -> ftvType bound t
   LambdaCPS _ tvars args body -> concatMap (ftvType (tvars ++ bound) . snd) args ++ ftv (tvars ++ bound) body
   TupleCPS _ vals -> concatMap (ftvVal bound . snd) vals
   TAppCPS void _ _ _ -> absurd void
   PackCPS void _ _ _ -> absurd void

ftvType :: [Id] -> TypeCPS V V V -> [Id]
ftvType bound t = case t of
   I32CPS -> []
   TVarCPS id -> ([id | id `notElem` bound])
   ArrowCPS tvars args -> concatMap (ftvType (tvars ++ bound)) args
   ProductCPS ts -> concatMap (ftvType bound . snd) ts
   ExistsCPS void _ _ -> absurd void
