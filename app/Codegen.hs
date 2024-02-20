{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License v. 2.0. If a copy of the MPL was not distributed with this
   file You can obtain one at https:--mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE GADTs #-}

module Codegen where
import Header
import Data.Word (Word8)
import Data.Map (Map, lookup, insert)
import Prelude hiding (lookup)
import Data.Void (absurd)
import Data.Bits (shiftR)


go :: [Stmt U] -> SLC [Word8]
go = undefined

data Op = ReqOp             -- 0x00
        | RegionOp          -- 0x01
        | HeapOp            -- 0x02
        | CapOp             -- 0x03
        | CapLEOp           -- 0x04
        | UniqueOp          -- 0x05
        | RWOp              -- 0x06
        | BothOp            -- 0x07
        | HandleOp          -- 0x08
        | I32Op             -- 0x09
        | EndFunctionOp     -- 0x0A
        | MutOp             -- 0x0B
        | TupleOp Int       -- 0x0C
        | ArrOp             -- 0x0D
        | AllOp             -- 0x0E
        | SomeOp            -- 0x0F
        | EmosOp            -- 0x10
        | FuncOp Int        -- 0x11
        | CTGetOp Int       -- 0x12
        | CTPopOp           -- 0x13
        | UnpackOp          -- 0x14
        | GetOp Int         -- 0x15
        | InitOp Int        -- 0x16
        | MallocOp          -- 0x17
        | ProjOp Int        -- 0x18
        | CallOp            -- 0x19
        | PrintOp           -- 0x1A
        | LitOp Int         -- 0x1B
        | GlobalFuncOp Int  -- 0x1C
        | HaltOp Int        -- 0x1D
        | PackOp            -- 0x1E
        | Word32Op          -- 0x1F
        | Word64Op          -- 0x20
        | PtrOp             -- 0x21
        | ReprsOp Int       -- 0x22
        | NewRgnOp          -- 0x23
        | FreeRgnOp         -- 0x24

type ReverseOps = [Op]

opsToBytes :: [Op] -> [Word8]
opsToBytes [] = []
opsToBytes (op:ops) = f op ++ opsToBytes ops
    where 
        f :: Op -> [Word8]
        f o = case o of
            ReqOp -> [0x00]
            RegionOp -> [0x01]
            HeapOp -> [0x02]
            CapOp -> [0x03]
            CapLEOp -> [0x04]
            UniqueOp -> [0x05]
            RWOp -> [0x06]
            BothOp -> [0x07]
            HandleOp -> [0x08]
            I32Op -> [0x09]
            EndFunctionOp -> [0x0A]
            MutOp -> [0x0B]
            TupleOp n -> [0x0C,fromIntegral n]
            ArrOp -> [0x0D]
            AllOp -> [0x0E]
            SomeOp -> [0x0F]
            EmosOp -> [0x10]
            FuncOp n -> [0x11,fromIntegral n]
            CTGetOp n -> [0x12,fromIntegral n]
            CTPopOp -> [0x13]
            UnpackOp -> [0x14]
            GetOp n -> [0x15,fromIntegral n]
            InitOp n -> [0x16,fromIntegral n]
            MallocOp -> [0x17]
            ProjOp n -> [0x18,fromIntegral n]
            CallOp -> [0x19]
            PrintOp -> [0x1A]
            LitOp n -> 
                [ 0x1B
                , shiftR 24 n
                , shiftR 16 n
                , shiftR 8 n
                , fromIntegral n
                ]
            GlobalFuncOp n -> 
                [ 0x1C
                , shiftR 24 n
                , shiftR 16 n
                , shiftR 8 n
                , fromIntegral n
                ]
            HaltOp n -> [0x1D,fromIntegral n]
            PackOp -> [0x1E]
            Word32Op -> [0x1F]
            Word64Op -> [0x20]
            PtrOp -> [0x21]
            ReprsOp n -> [0x22,fromIntegral n]
            NewRgnOp -> [0x23]
            FreeRgnOp -> [0x24]

goStmt :: Stmt U -> (Id, ReverseOps)
goStmt (Func x tvars params body) = undefined

goExpr :: Int -> Int -> Map Id Int -> Map Id Int -> ExprCPS U U U -> ReverseOps
goExpr rt_stack_size ct_stack_size rt_locals ct_locals e = case e of
    AppCPS f targs args -> 
        let (ct_stack_size2, targs_ops_rev) = foldr (\targ (i, ops_rev)-> (i+1, goType ct_stack_size ct_locals targ ++ ops_rev)) (ct_stack_size, []) targs in
        let (rt_stack_size2, args_ops_rev) = foldr (\arg (i, ops_rev)-> (i+1, goVal (rt_stack_size+1) ct_stack_size2 rt_locals ct_locals arg ++ ops_rev)) (rt_stack_size, []) args in
        let f_ops = goVal rt_stack_size2 ct_stack_size2 rt_locals ct_locals f in
            CallOp:f_ops++args_ops_rev++targs_ops_rev
    HaltCPS v -> HaltOp 0 : goVal rt_stack_size ct_stack_size rt_locals ct_locals v
    LetCPS x _t v scope -> 
        let v_ops = goVal rt_stack_size ct_stack_size rt_locals ct_locals v in
        let scope_ops = goExpr (rt_stack_size+1) ct_stack_size (insert x (rt_stack_size+1) rt_locals) ct_locals scope in
        scope_ops++v_ops
    TupleProjCPS x tpl i scope -> 
        let tpl_ops = goVal rt_stack_size ct_stack_size rt_locals ct_locals tpl in
        let scope_ops = goExpr (rt_stack_size+1) ct_stack_size (insert x (rt_stack_size+1) rt_locals) ct_locals scope in
        scope_ops++ProjOp i:tpl_ops
    UnpackCPS () x _y v scope -> 
        let v_ops = goVal rt_stack_size ct_stack_size rt_locals ct_locals v in
        let scope_ops = goExpr (rt_stack_size+1) ct_stack_size (insert x (rt_stack_size+1) rt_locals) ct_locals scope in
        scope_ops++UnpackOp:v_ops
    MallocCPS () x ts scope -> 
        let (ct_stack_size2, ts_ops) = foldr (\t (i, ops_rev)-> (i+1, goType i ct_locals t ++ ops_rev)) (ct_stack_size, []) ts in
        let scope_ops = goExpr (rt_stack_size+1) ct_stack_size2 (insert x (rt_stack_size+1) rt_locals) ct_locals scope in
        scope_ops++MallocOp:ts_ops
    InitCPS () x tpl i v scope -> 
        let tpl_ops = goVal rt_stack_size ct_stack_size rt_locals ct_locals tpl in
        let v_ops = goVal (rt_stack_size+1) ct_stack_size rt_locals ct_locals v in
        let scope_ops = goExpr (rt_stack_size+1) ct_stack_size (insert x (rt_stack_size+1) rt_locals) ct_locals scope in
        scope_ops++InitOp i:v_ops++tpl_ops

goVal :: Int -> Int -> Map Id Int -> Map Id Int -> ValCPS U U U -> ReverseOps
goVal rt_stack_size ct_stack_size rt_locals ct_locals v = case v of
    LitCPS i -> [LitOp i]
    VarCPS x _t -> case lookup x rt_locals of
        Just pos -> [GetOp $ rt_stack_size - pos]
        Nothing -> undefined
    LambdaCPS (NotTrue void) _ _ _ -> absurd void
    TupleCPS (NotTrue void) _ -> absurd void
    TAppCPS () _ _ _ -> undefined
    PackCPS () t v2 exist_t -> 
        let v_ops_rev = goVal rt_stack_size ct_stack_size rt_locals ct_locals v2 in
        let exist_t_ops_rev = goType ct_stack_size ct_locals exist_t in
        let t_ops_rev = goType (ct_stack_size+1) ct_locals t in
        PackOp:t_ops_rev++exist_t_ops_rev++v_ops_rev

goType :: Int -> Map Id Int -> TypeCPS U U U -> ReverseOps
goType ct_stack_size ct_locals t = case t of
    I32CPS -> [I32Op]
    TVarCPS x -> case lookup x ct_locals of
        Just pos -> [CTGetOp $ ct_stack_size - pos]
        Nothing -> undefined
    ArrowCPS [] params -> 
        let (_, param_ops_rev) = foldr (\t2 (i, ops_rev)->let o = goType i ct_locals t2 in (i+1,o++ops_rev)) (ct_stack_size, [RWOp, HeapOp]) params in
        FuncOp (length params) : param_ops_rev
    ArrowCPS (_:_) _ -> undefined
    ProductCPS ts -> 
        let (_, ts_ops_rev) = foldr (\(_,t2) (i, ops_rev)->let o = goType i ct_locals t2 in (i+1,o++ops_rev)) (ct_stack_size, [HeapOp]) ts in
        TupleOp (length ts) : ts_ops_rev
    ExistsCPS () x body -> 
        let body_ops_rev = goType (ct_stack_size + 1) (insert x (ct_stack_size + 1) ct_locals) body in
        EmosOp : body_ops_rev ++ [SomeOp]