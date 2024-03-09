{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License v. 2.0. If a copy of the MPL was not distributed with this
   file You can obtain one at https:--mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE GADTs #-}

module Codegen (go) where

import Data.Bits (shiftR)
import Data.Map (Map, empty)
import qualified Data.Map as Map
import Data.Void (absurd)
import Data.Word (Word32, Word8)
import Debug.Trace (trace)
import Header
import Prelude hiding (id, lookup)

go :: [Stmt U U] -> SLC [Word8]
go stmts =
  let (_, stmts_ops_rev, func_renames) =
        foldr
          ( \stmt (instr_pos, ops_rev, renames) ->
              trace (prettyStmt stmt) $
                let (id, stmt_ops_rev) = goStmt stmt
                 in (instr_pos + length stmt_ops_rev, stmt_ops_rev ++ ops_rev, Map.insert id instr_pos renames)
          )
          (0, [], empty)
          stmts
   in let stmts_ops = NewRgnOp : GlobalFuncOp (-1) : CallOp : EndFunctionOp : reverse stmts_ops_rev
       in return $
            trace (show stmts_ops) $
              (\bytes -> 0 : 0 : 0 : 0 : bytes) $
                foldr
                  ( \op ops ->
                      case op of
                        GlobalFuncOp id ->
                          case Map.lookup id func_renames of
                            Just new_id -> opToBytes (GlobalFuncOp new_id) ++ ops
                            Nothing -> undefined
                        _ -> opToBytes op ++ ops
                  )
                  []
                  stmts_ops

data RT

data CT

newtype Locals a = Locals (Map Id (Pos a)) deriving (Show)

newtype Size a = Size {sizeToInt :: Int} deriving (Show)

newtype Pos a = Pos {posToInt :: Int} deriving (Show)

insert :: Id -> Pos a -> Locals a -> Locals a
insert k v (Locals m) = Locals (Map.insert k v m)

lookup :: Id -> Locals a -> Maybe (Pos a)
lookup k (Locals m) = Map.lookup k m

inc :: Size a -> Size a
inc (Size s) = Size (s + 1)

dec :: Size a -> Size a
dec (Size s) = Size (s - 1)

diff :: Pos a -> Pos a -> Pos a
diff (Pos i) (Pos j) = Pos (i - j)

getLastOf :: Size a -> Pos a
getLastOf (Size s) = Pos (s - 1)

data Op
  = ReqOp -- 0x00
  | RegionOp -- 0x01
  | HeapOp -- 0x02
  | CapOp -- 0x03
  | CapLEOp -- 0x04
  | UniqueOp -- 0x05
  | RWOp -- 0x06
  | BothOp -- 0x07
  | HandleOp -- 0x08
  | I32Op -- 0x09
  | EndFunctionOp -- 0x0A
  | MutOp -- 0x0B
  | TupleOp Int -- 0x0C
  | ArrOp -- 0x0D
  | AllOp -- 0x0E
  | SomeOp -- 0x0F
  | EmosOp -- 0x10
  | FuncOp Int -- 0x11
  | CTGetOp (Pos CT) -- 0x12
  | CTPopOp -- 0x13
  | UnpackOp -- 0x14
  | GetOp (Pos RT) -- 0x15
  | InitOp Int -- 0x16
  | MallocOp -- 0x17
  | ProjOp Int -- 0x18
  | CallOp -- 0x19
  | PrintOp -- 0x1A
  | LitOp Int -- 0x1B
  | GlobalFuncOp Id -- 0x1C
  | HaltOp Int -- 0x1D
  | PackOp -- 0x1E
  | Word32Op -- 0x1F
  | Word64Op -- 0x20
  | PtrOp -- 0x21
  | ReprsOp Int -- 0x22
  | NewRgnOp -- 0x23
  | FreeRgnOp -- 0x24
  | ForallOp -- 0x25
  | LlarofOp -- 0x26
  deriving (Show)

type ReverseOps = [Op]

opToBytes :: Op -> [Word8]
opToBytes op = case op of
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
  TupleOp n -> [0x0C, fromIntegral n]
  ArrOp -> [0x0D]
  AllOp -> [0x0E]
  SomeOp -> [0x0F]
  EmosOp -> [0x10]
  FuncOp n -> [0x11, fromIntegral n]
  CTGetOp n -> [0x12, fromIntegral $ posToInt n]
  CTPopOp -> [0x13]
  UnpackOp -> [0x14]
  GetOp n -> [0x15, fromIntegral $ posToInt n]
  InitOp n -> [0x16, fromIntegral n]
  MallocOp -> [0x17]
  ProjOp n -> [0x18, fromIntegral n]
  CallOp -> [0x19]
  PrintOp -> [0x1A]
  LitOp n ->
    [ 0x1B,
      fromIntegral $ shiftR (fromIntegral n :: Word32) 24,
      fromIntegral $ shiftR (fromIntegral n :: Word32) 16,
      fromIntegral $ shiftR (fromIntegral n :: Word32) 8,
      fromIntegral n
    ]
  GlobalFuncOp n ->
    [ 0x1C,
      fromIntegral $ shiftR (fromIntegral n :: Word32) 24,
      fromIntegral $ shiftR (fromIntegral n :: Word32) 16,
      fromIntegral $ shiftR (fromIntegral n :: Word32) 8,
      fromIntegral n
    ]
  HaltOp n -> [0x1D, fromIntegral n]
  PackOp -> [0x1E]
  Word32Op -> [0x1F]
  Word64Op -> [0x20]
  PtrOp -> [0x21]
  ReprsOp n -> [0x22, fromIntegral n]
  NewRgnOp -> [0x23]
  FreeRgnOp -> [0x24]
  ForallOp -> [0x25]
  LlarofOp -> [0x26]

goStmt :: Stmt U U -> (Id, ReverseOps)
goStmt (Func id tvars params body) =
  let (ct_stack_size, tvar_ops_rev, ct_locals) =
        foldr
          ( \x (i, ops_rev, locals) ->
              let new_i = inc i
               in (new_i, AllOp : PtrOp : ops_rev, insert x (getLastOf new_i) locals)
          )
          (Size 1, [], Locals empty)
          (reverse $ ids tvars)
   in let (_, params_ops_rev, rt_locals) =
            foldr
              ( \(x, t) (i, ops_rev, locals) ->
                  (dec i, ReqOp : goType ct_stack_size ct_locals t ++ ops_rev, insert x (getLastOf i) locals)
              )
              (Size (length params + 1), [], Locals empty)
              (reverse params)
       in let body_ops = goExpr (inc $ Size $ length params) (inc ct_stack_size) (trace (show rt_locals) rt_locals) ct_locals body
           in (id, EndFunctionOp : body_ops ++ params_ops_rev ++ tvar_ops_rev)

goExpr :: Size RT -> Size CT -> Locals RT -> Locals CT -> ExprCPS U U U U -> ReverseOps
goExpr rt_stack_size ct_stack_size rt_locals ct_locals e = case e of
  AppCPS f targs args ->
    let (ct_stack_size2, targs_ops_rev) = foldr (\targ (i, ops_rev) -> (inc i, goCTArg ct_stack_size ct_locals targ ++ ops_rev)) (ct_stack_size, []) targs
     in let (rt_stack_size2, args_ops_rev) = foldr (\arg (i, ops_rev) -> (inc i, goVal i ct_stack_size2 rt_locals ct_locals arg ++ ops_rev)) (rt_stack_size, []) args
         in let f_ops = goVal rt_stack_size2 ct_stack_size2 rt_locals ct_locals f
             in CallOp : f_ops ++ args_ops_rev ++ targs_ops_rev
  HaltCPS v ->
    HaltOp 0 : goVal rt_stack_size ct_stack_size rt_locals ct_locals v
  LetCPS x _t v scope ->
    let v_ops = goVal rt_stack_size ct_stack_size rt_locals ct_locals v
     in let rt_stack_size2 = inc rt_stack_size
         in let scope_ops = goExpr rt_stack_size2 ct_stack_size (insert x (getLastOf rt_stack_size2) rt_locals) ct_locals scope
             in scope_ops ++ v_ops
  TupleProjCPS x tpl i scope ->
    let tpl_ops = goVal rt_stack_size ct_stack_size rt_locals ct_locals tpl
     in let rt_stack_size2 = inc rt_stack_size
         in let scope_ops = goExpr rt_stack_size2 ct_stack_size (insert x (getLastOf rt_stack_size2) rt_locals) ct_locals scope
             in scope_ops ++ ProjOp i : tpl_ops
  UnpackCPS () _x y v scope ->
    let v_ops = goVal rt_stack_size ct_stack_size rt_locals ct_locals v
     in let rt_stack_size2 = inc rt_stack_size
         in let scope_ops = goExpr rt_stack_size2 ct_stack_size (insert y (getLastOf rt_stack_size2) rt_locals) ct_locals scope
             in scope_ops ++ UnpackOp : v_ops
  MallocCPS () x ts (Rgn handle) scope ->
    let handle_ops = goVal rt_stack_size ct_stack_size rt_locals ct_locals (VarCPS handle undefined False)
     in let (ct_stack_size2, ts_ops) = foldr (\t (i, ops_rev) -> (inc i, goType i ct_locals t ++ ops_rev)) (ct_stack_size, []) ts
         in let rt_stack_size2 = inc rt_stack_size -- push handle, pop handle, push tuple
             in let scope_ops = goExpr rt_stack_size2 ct_stack_size2 (insert x (getLastOf rt_stack_size2) rt_locals) ct_locals scope
                 in scope_ops ++ MallocOp : TupleOp (length ts_ops) : ts_ops ++ handle_ops
  InitCPS () x tpl i v scope ->
    let tpl_ops = goVal rt_stack_size ct_stack_size rt_locals ct_locals tpl
     in let rt_stack_size2 = inc rt_stack_size -- push
         in let v_ops = goVal rt_stack_size2 ct_stack_size rt_locals ct_locals v
             in let rt_stack_size3 = inc rt_stack_size2 -- push
                 in let rt_stack_size4 = dec rt_stack_size3 -- pop
                     in let scope_ops = goExpr rt_stack_size4 ct_stack_size (insert x (getLastOf rt_stack_size4) rt_locals) ct_locals scope
                         in scope_ops ++ InitOp i : v_ops ++ tpl_ops

goVal :: Size RT -> Size CT -> Locals RT -> Locals CT -> ValCPS U U U U -> ReverseOps
goVal rt_stack_size ct_stack_size rt_locals ct_locals v = case v of
  LitCPS i -> [LitOp i]
  VarCPS x _t global ->
    if global
      then
        [GlobalFuncOp x]
      else
        rtGet rt_stack_size rt_locals x
  LambdaCPS (NotTrue void) _ _ _ -> absurd void
  TupleCPS _ (NotTrue void) _ -> absurd void
  TAppCPS () _ _ _ -> undefined
  PackCPS () t v2 exist_t ->
    let v_ops_rev = goVal rt_stack_size ct_stack_size rt_locals ct_locals v2
     in let exist_t_ops_rev = goType ct_stack_size ct_locals exist_t
         in let t_ops_rev = goType (inc ct_stack_size) ct_locals t
             in PackOp : t_ops_rev ++ exist_t_ops_rev ++ v_ops_rev

goType :: Size CT -> Locals CT -> TypeCPS U U U U -> ReverseOps
goType ct_stack_size ct_locals t = case t of
  I32CPS -> [I32Op]
  TVarCPS x -> ctGet ct_stack_size ct_locals x
  ArrowCPS kind_ctx params ->
    let (ct_locals2, ct_stack_size2, kind_ctx_ops) = foldr (\entry (locals, i, ops_rev) -> (insert (getId entry) (getLastOf $ inc i) locals, inc i, ForallOp : ops_rev)) (ct_locals, ct_stack_size, []) kind_ctx
     in let (_, param_ops_rev) = foldr (\t2 (i, ops_rev) -> let o = goType i ct_locals2 t2 in (inc i, o ++ ops_rev)) (ct_stack_size2, []) params
         in let close_forall_ops = replicate (length kind_ctx) LlarofOp
             in close_forall_ops ++ FuncOp (length params) : param_ops_rev ++ kind_ctx_ops
  ProductCPS (Rgn id) ts ->
    let r_ops = ctGet ct_stack_size ct_locals id
     in let (_, ts_ops_rev) = foldr (\(_, t2) (i, ops_rev) -> let o = goType i ct_locals t2 in (inc i, o ++ ops_rev)) (inc ct_stack_size, r_ops) ts
         in TupleOp (length ts) : ts_ops_rev
  ExistsCPS () x body ->
    let ct_stack_size2 = inc ct_stack_size
     in let body_ops_rev = goType ct_stack_size2 (insert x (getLastOf ct_stack_size2) ct_locals) body
         in EmosOp : body_ops_rev ++ [SomeOp, PtrOp]
  HandleTypeCPS () id -> HandleOp : ctGet ct_stack_size ct_locals id

rtGet :: Size RT -> Locals RT -> Id -> [Op]
rtGet rt_stack_size rt_locals id =
  case lookup id rt_locals of
    Just pos -> [GetOp $ getLastOf rt_stack_size `diff` pos]
    Nothing -> undefined

ctGet :: Size CT -> Locals CT -> Id -> [Op]
ctGet ct_stack_size ct_locals id =
  case lookup id ct_locals of
    Just pos -> [CTGetOp $ getLastOf ct_stack_size `diff` pos]
    Nothing -> undefined

goCTArg :: Size CT -> Locals CT -> CTArg U U U U -> ReverseOps
goCTArg ct_stack_size ct_locals a = case a of
  TypeCTArg t -> goType ct_stack_size ct_locals t
  RgnCTArg () (Rgn id) -> ctGet ct_stack_size ct_locals id
  CapCTArg () _c -> undefined