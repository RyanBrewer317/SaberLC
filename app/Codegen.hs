{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Codegen (go) where
import Header (SLC, StmtR (FuncR), TypeR (HandleTypeR, I32R, TypeVarR, FunctionTypeR, TupleTypeR, ExistentialR, ForallR, ForallRegionR), ExprR (AppR, HaltR, ProjR, UnpackR, MallocR, InitR), ValR (VarR, IntLitR, TypeAppR, PackR, TypeLambdaR, RegionLambdaR, RegionAppR), Ident (..))
import Data.Binary (Word8, Word32)
import Data.Map (Map, empty)
import qualified Data.Map as Map
import Data.Bits (shiftR)
import Prelude hiding (lookup)
import GHC.IO (unsafePerformIO)

go :: [StmtR] -> SLC [Word8]
go stmts =
  let (_, stmts_ops, t_ops, func_renames) =
        foldr
          (\stmt (instr_pos, ops, tops, renames) ->
            let (idNum, t_ops_rev, stmt_ops_rev) = goStmt stmt in
            (instr_pos + length stmt_ops_rev + 1, ops ++ reverse stmt_ops_rev, tops ++ reverse (LcedOp : t_ops_rev), Map.insert idNum instr_pos renames)
          )
          (4, [], [], empty) -- 4 because of the function prepended in stmts_ops2
          (reverse stmts) in
  let fn_num = length stmts in
  let fn_num_ops = reverse [
        fromIntegral $ shiftR (fromIntegral fn_num :: Word32) 24,
        fromIntegral $ shiftR (fromIntegral fn_num :: Word32) 16,
        fromIntegral $ shiftR (fromIntegral fn_num :: Word32) 8,
        fromIntegral (fromIntegral fn_num :: Word32)]
    in
  let stmts_ops2 = [NewRgnOp 4096, GlobalFuncOp (-1), CallOp] ++ stmts_ops in
  return $
    unsafePerformIO (writeFile "t.txt" (unlines $ show (length stmts) : map show t_ops ++ map show stmts_ops2) >> return id) $
    (\bytes -> 0 : 0 : 0 : 0 : fn_num_ops ++ concatMap opToBytes t_ops ++ bytes) $
    foldr
      (\op ops ->
          case op of
            GlobalFuncOp idNum ->
              case Map.lookup idNum func_renames of
                Just new_id -> opToBytes (GlobalFuncOp new_id) ++ ops
                Nothing -> undefined
            _ -> opToBytes op ++ ops
      )
      []
      stmts_ops2

data RT

data CT

newtype Locals a = Locals (Map Int (Pos a)) deriving (Show)

newtype Size a = Size {sizeToInt :: Int} deriving (Show)

newtype Pos a = Pos {posToInt :: Int} deriving (Show)

insert :: Int -> Pos a -> Locals a -> Locals a
insert k v (Locals m) = Locals (Map.insert k v m)

lookup :: Int -> Locals a -> Maybe (Pos a)
lookup k (Locals m) = Map.lookup k m

inc :: Size a -> Size a
inc (Size s) = Size (s + 1)

dec :: Size a -> Size a
dec (Size s) = Size (s - 1)

diff :: Pos a -> Pos a -> Pos a
diff (Pos i) (Pos j) = Pos (i - j)

getLastOf :: Size a -> Pos a
getLastOf (Size s) = Pos (s - 1)

getId :: Ident -> Int
getId (Local i _) = i
getId (Global _) = undefined

data Op
  = UniqueOp -- 0x00
  | HandleOp -- 0x01
  | I32Op -- 0x02
  | TupleOp Int -- 0x03
  | SomeOp -- 0x04
  | AllOp -- 0x05
  | RgnOp -- 0x06
  | EndOp -- 0x07
  | AppOp -- 0x08
  | FuncOp Int -- 0x09
  | CTGetOp (Pos CT) -- 0x0A
  | LcedOp -- 0x0B
  | UnpackOp -- 0x0C
  | GetOp (Pos RT) -- 0x0D
  | InitOp Int -- 0x0E
  | MallocOp -- 0x0F
  | ProjOp Int -- 0x10
  | CallOp -- 0x11
  | PrintOp -- 0x12
  | LitOp Int -- 0x13
  | GlobalFuncOp Int -- 0x14
  | HaltOp Int -- 0x15
  | PackOp -- 0x16
  | SizeOp Int -- 0x17
  | NewRgnOp Int -- 0x18
  | FreeRgnOp -- 0x19
  | PtrOp -- 0x1A
  | DerefOp -- 0x1B
  deriving (Show)

type ReverseOps = [Op]

opToBytes :: Op -> [Word8]
opToBytes op = case op of
  UniqueOp -> [0x00]
  HandleOp -> [0x01]
  I32Op -> [0x02]
  TupleOp n -> [0x03, fromIntegral n]
  SomeOp -> [0x04]
  AllOp -> [0x05]
  RgnOp -> [0x06]
  EndOp -> [0x07]
  AppOp -> [0x08]
  FuncOp n -> [0x09, fromIntegral n]
  CTGetOp pos -> [0x0A, fromIntegral $ posToInt pos]
  LcedOp -> [0x0B]
  UnpackOp -> [0x0C]
  GetOp pos -> [0x0D, fromIntegral $ posToInt pos]
  InitOp i -> [0x0E, fromIntegral i]
  MallocOp -> [0x0F]
  ProjOp i -> [0x10, fromIntegral i]
  CallOp -> [0x11]
  PrintOp -> [0x12]
  LitOp n -> 0x13 : reverse [
      fromIntegral $ shiftR (fromIntegral n :: Word32) 24,
      fromIntegral $ shiftR (fromIntegral n :: Word32) 16,
      fromIntegral $ shiftR (fromIntegral n :: Word32) 8,
      fromIntegral n
    ]
  GlobalFuncOp n -> 0x14 : reverse [
      fromIntegral $ shiftR (fromIntegral n :: Word32) 24,
      fromIntegral $ shiftR (fromIntegral n :: Word32) 16,
      fromIntegral $ shiftR (fromIntegral n :: Word32) 8,
      fromIntegral n
    ]
  HaltOp n -> [0x15, fromIntegral n]
  PackOp -> [0x16]
  SizeOp n -> 0x17 : reverse [
      fromIntegral $ shiftR (fromIntegral n :: Word32) 24,
      fromIntegral $ shiftR (fromIntegral n :: Word32) 16,
      fromIntegral $ shiftR (fromIntegral n :: Word32) 8,
      fromIntegral n
    ]
  NewRgnOp n -> 0x18 : reverse [
      fromIntegral $ shiftR (fromIntegral n :: Word32) 24,
      fromIntegral $ shiftR (fromIntegral n :: Word32) 16,
      fromIntegral $ shiftR (fromIntegral n :: Word32) 8,
      fromIntegral n
    ]
  FreeRgnOp -> [0x19]
  PtrOp -> [0x1A]
  DerefOp -> [0x1B]

goStmt :: StmtR -> (Int, ReverseOps, ReverseOps)
goStmt (FuncR idNum _ ctx params body) =
  let (ct_stack_size, tvar_ops_rev, ct_locals) =
        foldr
          (\(x, isRgn, _) (i, ops_rev, locals) ->
            let new_i = inc i in
            if isRgn then
              (new_i, RgnOp : ops_rev, insert x (getLastOf new_i) locals)
            else
              (new_i, AllOp : SizeOp 16 : ops_rev, insert x (getLastOf new_i) locals)
          )
          (Size 0, [], Locals empty)
          (reverse ctx) in
  let (_, params_ops_rev, rt_locals) =
        foldr
          (\(x, _, t) (i, ops_rev, locals) ->
            (dec i, goType ct_stack_size ct_locals t ++ ops_rev, insert x (getLastOf i) locals)
          )
          (Size (length params + 1), [], Locals empty)
          (reverse params) in
  let t_ops = replicate (length ctx) EndOp ++ FuncOp (length params) : params_ops_rev ++ tvar_ops_rev in
  let body_ops = goExpr (inc $ Size $ length params) ct_stack_size rt_locals ct_locals body in
    (idNum, t_ops, body_ops)

goExpr :: Size RT -> Size CT -> Locals RT -> Locals CT -> ExprR -> ReverseOps
goExpr rt_stack_size ct_stack_size rt_locals ct_locals e = case e of
  AppR f args ->
    let (rt_stack_size2, args_ops_rev) = foldr (\arg (i, ops_rev) -> (inc i, goVal i ct_stack_size rt_locals ct_locals arg ++ ops_rev)) (rt_stack_size, []) args in
    let f_ops = goVal rt_stack_size2 ct_stack_size rt_locals ct_locals f in
    CallOp : f_ops ++ args_ops_rev
  HaltR v ->
    HaltOp 0 : goVal rt_stack_size ct_stack_size rt_locals ct_locals v
  ProjR x _ _ tpl i scope ->
    let tpl_ops = goVal rt_stack_size ct_stack_size rt_locals ct_locals tpl in
    let rt_stack_size2 = inc rt_stack_size in
    let scope_ops = goExpr rt_stack_size2 ct_stack_size (insert x (getLastOf rt_stack_size2) rt_locals) ct_locals scope in
    scope_ops ++ ProjOp i : tpl_ops
  UnpackR _ _ y _ v scope ->
    let v_ops = goVal rt_stack_size ct_stack_size rt_locals ct_locals v in
    let rt_stack_size2 = inc rt_stack_size in
    let scope_ops = goExpr rt_stack_size2 ct_stack_size (insert y (getLastOf rt_stack_size2) rt_locals) ct_locals scope in
    scope_ops ++ UnpackOp : v_ops
  MallocR x _ ts r handle scope ->
    let handle_ops = goVal rt_stack_size ct_stack_size rt_locals ct_locals (VarR (HandleTypeR r) False handle) in
    let r_ops = ctGet ct_stack_size ct_locals $ getId r in
    let ct_stack_size2 = inc ct_stack_size in -- push r
    let (_, ts_ops) = foldr (\t (i, ops_rev) -> (inc i, goType i ct_locals t ++ ops_rev)) (ct_stack_size2, []) ts in
    let rt_stack_size2 = inc rt_stack_size in -- push handle, pop handle, push tuple 
    let scope_ops = goExpr rt_stack_size2 ct_stack_size (insert x (getLastOf rt_stack_size2) rt_locals) ct_locals scope in -- ct_stack_size because the ct stack should be back where it started now
    scope_ops ++ MallocOp : TupleOp (length ts) : ts_ops ++ r_ops ++ handle_ops
  InitR x _ tpl i v scope ->
    let tpl_ops = goVal rt_stack_size ct_stack_size rt_locals ct_locals tpl in
    let rt_stack_size2 = inc rt_stack_size in -- push
    let v_ops = goVal rt_stack_size2 ct_stack_size rt_locals ct_locals v in
    let rt_stack_size3 = inc rt_stack_size2 in -- push
    let rt_stack_size4 = dec rt_stack_size3 in -- pop
    let scope_ops = goExpr rt_stack_size4 ct_stack_size (insert x (getLastOf rt_stack_size4) rt_locals) ct_locals scope in
    scope_ops ++ InitOp i : v_ops ++ tpl_ops

goVal :: Size RT -> Size CT -> Locals RT -> Locals CT -> ValR -> ReverseOps
goVal rt_stack_size ct_stack_size rt_locals ct_locals v = case v of
  IntLitR i -> [LitOp i]
  VarR _t global x ->
    if global
      then
        let idNum = case x of
              Local i _ -> i
              Global _ -> undefined
          in
        [GlobalFuncOp idNum]
      else
        rtGet rt_stack_size rt_locals $ getId x
  TypeAppR _t f targs ->
    let (_, targs_ops) = foldr (\t (i, ops_rev) -> (inc i, goType i ct_locals t ++ ops_rev)) (ct_stack_size, []) targs in
    let f_ops = goVal rt_stack_size ct_stack_size rt_locals ct_locals f in
      AppOp : f_ops ++ targs_ops
  RegionAppR f rgnargs ->
    let (_, rgnargs_ops) = foldr (\rgnarg (i, ops_rev) -> (inc i, ctGet i ct_locals (getId rgnarg) ++ ops_rev)) (ct_stack_size, []) rgnargs in
    let f_ops = goVal rt_stack_size ct_stack_size rt_locals ct_locals f in
      AppOp : f_ops ++ rgnargs_ops
  PackR t v2 exist_t -> do
    let v_ops_rev = goVal rt_stack_size ct_stack_size rt_locals ct_locals v2
    let exist_t_ops_rev = goType ct_stack_size ct_locals exist_t
    let t_ops_rev = goType (inc ct_stack_size) ct_locals t
    PackOp : t_ops_rev ++ exist_t_ops_rev ++ v_ops_rev
  TypeLambdaR {} -> undefined
  RegionLambdaR {} -> undefined

goType :: Size CT -> Locals CT -> TypeR -> ReverseOps
goType ct_stack_size ct_locals t = case t of
  I32R -> [I32Op]
  TypeVarR x -> ctGet ct_stack_size ct_locals $ getId x
  FunctionTypeR params -> do
    let (_, param_ops_rev) =
          foldr
            ( \t2 (i, ops_rev) ->
                let o = goType i ct_locals t2 in
                (inc i, o ++ ops_rev)
            )
            (ct_stack_size, [])
            params
    FuncOp (length params) : param_ops_rev
  ForallR kind_ctx body -> do
    let (ct_locals2, ct_stack_size2, kind_ctx_ops) =
          foldr
            ( \(x, _) (locals, i, ops_rev) -> do
                let o = AllOp : SizeOp 16 : ops_rev
                let new_i = inc i
                let new_locals = insert x (getLastOf new_i) locals
                (new_locals, new_i, o)
            )
            (ct_locals, ct_stack_size, [])
            kind_ctx
    let body_ops = goType ct_stack_size2 ct_locals2 body
    replicate (length kind_ctx) EndOp ++ body_ops ++ kind_ctx_ops
  ForallRegionR kind_ctx body -> do
    let (ct_locals2, ct_stack_size2, kind_ctx_ops) =
          foldr
            ( \(x, _, owned) (locals, i, ops_rev) -> do
                let o = if owned then UniqueOp : RgnOp : ops_rev else RgnOp : ops_rev
                let new_i = inc i
                let new_locals = insert x (getLastOf new_i) locals
                (new_locals, new_i, o)
            )
            (ct_locals, ct_stack_size, [])
            kind_ctx
    let body_ops = goType ct_stack_size2 ct_locals2 body
    replicate (length kind_ctx) EndOp ++ body_ops ++ kind_ctx_ops
  TupleTypeR r ts ->
    let r_ops = ctGet ct_stack_size ct_locals $ getId r
     in let (_, ts_ops_rev) = foldr (\t2 (i, ops_rev) -> let o = goType i ct_locals t2 in (inc i, o ++ ops_rev)) (inc ct_stack_size, r_ops) ts
         in TupleOp (length ts) : ts_ops_rev
  ExistentialR x _ body ->
    let ct_stack_size2 = inc ct_stack_size
     in let body_ops_rev = goType ct_stack_size2 (insert x (getLastOf ct_stack_size2) ct_locals) body
         in EndOp : body_ops_rev ++ [SomeOp, SizeOp 16]
  HandleTypeR r -> HandleOp : ctGet ct_stack_size ct_locals (getId r)

rtGet :: Size RT -> Locals RT -> Int -> [Op]
rtGet rt_stack_size rt_locals ident =
  case lookup ident rt_locals of
    Just pos -> [GetOp $ getLastOf rt_stack_size `diff` pos]
    Nothing -> error $ show ident ++ " -- " ++ show rt_locals

ctGet :: Size CT -> Locals CT -> Int -> [Op]
ctGet ct_stack_size ct_locals ident =
  case lookup ident ct_locals of
    Just pos -> [CTGetOp $ getLastOf ct_stack_size `diff` pos]
    Nothing -> undefined
