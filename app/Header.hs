{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}

module Header where
import Prelude hiding (id)
import Data.List (intercalate)
import Data.Bifunctor (second)
import Data.Void (Void, absurd)

type U = ()
type V = Void

data TypeSyntax = I32Syntax
                | TVarSyntax String
                | ForallSyntax String TypeSyntax
                | ArrowSyntax TypeSyntax TypeSyntax
                deriving (Show, Eq)

data ExprSyntax = LambdaSyntax String TypeSyntax ExprSyntax
                | TLambdaSyntax String ExprSyntax
                | AppSyntax ExprSyntax ExprSyntax
                | TAppSyntax ExprSyntax TypeSyntax
                | VarSyntax String
                | LitSyntax Int
                deriving (Show, Eq)

type Id = Int

data Type = I32
          | TVar Id
          | Forall Id Type
          | Arrow Type Type
          deriving (Show, Eq)

data Expr = Lambda Id Type Expr
          | TLambda Id Expr
          | App Type Expr Expr
          | TApp Type Expr Type
          | Var Id Type Bool
          | Lit Int
          deriving (Show, Eq)

data TypeCPS clos hoist alloc = I32CPS
                              | TVarCPS Id
                              | ArrowCPS [Id] [TypeCPS clos hoist alloc]
                              | ProductCPS [(Alloc alloc, TypeCPS clos hoist alloc)]
                              | ExistsCPS clos Id (TypeCPS clos hoist alloc)
                              deriving (Show, Eq)

data ValCPS clos hoist alloc = LitCPS Int
                             | VarCPS Id (TypeCPS clos hoist alloc) Bool
                             | LambdaCPS (Not hoist) [Id] [(Id, TypeCPS clos hoist alloc)] (ExprCPS clos hoist alloc)
                             | TupleCPS (Not alloc) [(Alloc alloc, ValCPS clos hoist alloc)]
                             | TAppCPS clos (TypeCPS clos hoist alloc) (ValCPS clos hoist alloc) [TypeCPS clos hoist alloc]
                             | PackCPS clos (TypeCPS clos hoist alloc) (ValCPS clos hoist alloc) (TypeCPS clos hoist alloc)
                             deriving (Show, Eq)

data ExprCPS clos hoist alloc = AppCPS (ValCPS clos hoist alloc) [TypeCPS clos hoist alloc] [ValCPS clos hoist alloc]
                              | HaltCPS (ValCPS clos hoist alloc)
                              | LetCPS Id (TypeCPS clos hoist alloc) (ValCPS clos hoist alloc) (ExprCPS clos hoist alloc)
                              | TupleProjCPS Id (ValCPS clos hoist alloc) Int (ExprCPS clos hoist alloc)
                              | UnpackCPS clos Id Id (ValCPS clos hoist alloc) (ExprCPS clos hoist alloc)
                              | MallocCPS alloc Id [TypeCPS clos hoist alloc] (ExprCPS clos hoist alloc)
                              | InitCPS alloc Id (ValCPS clos hoist alloc) Int (ValCPS clos hoist alloc) (ExprCPS clos hoist alloc)
                              deriving (Show, Eq)

data Alloc alloc where
   Initialized :: Alloc U
   Uninitialized :: Alloc U
   PreAlloc :: Alloc V

instance Show (Alloc alloc) where
   show Initialized = "Initialized"
   show Uninitialized = "Uninitialized"
   show PreAlloc = "PreAlloc"

instance Eq (Alloc alloc) where
   (==) Initialized Initialized = True
   (==) Uninitialized Uninitialized = True
   (==) PreAlloc PreAlloc = True
   (==) _ _ = False

data Not p where
   NotFalse :: Not V
   NotTrue :: V -> Not U

instance Show (Not p) where
   show NotFalse = "NotFalse"
   show (NotTrue v) = absurd v
instance Eq (Not p) where
   (==) NotFalse NotFalse = True
   (==) (NotTrue v) _ = absurd v

getType :: Expr -> Type
getType (Lambda _ t e) = Arrow t (getType e)
getType (TLambda x e) = Forall x (getType e)
getType (App t _ _) = t
getType (TApp t _ _) = t
getType (Var _ t _) = t
getType (Lit _) = I32

getTypeCPS :: ExprCPS clos hoist alloc -> TypeCPS clos hoist alloc
getTypeCPS (AppCPS {}) = undefined
getTypeCPS (HaltCPS v) = getValCPSType v
getTypeCPS (LetCPS _ _ _ e) = getTypeCPS e
getTypeCPS (TupleProjCPS _ _ _ e) = getTypeCPS e
getTypeCPS (UnpackCPS _ _ _ _ e) = getTypeCPS e
getTypeCPS (MallocCPS _ _ _ e) = getTypeCPS e
getTypeCPS (InitCPS _ _ _ _ _ e) = getTypeCPS e

getValCPSType :: ValCPS clos hoist alloc -> TypeCPS clos hoist alloc
getValCPSType (LitCPS _) = I32CPS
getValCPSType (VarCPS _ t _) = t
getValCPSType (LambdaCPS _ tvars ts _) = ArrowCPS tvars (map snd ts)
getValCPSType (TupleCPS _ vs) = ProductCPS (map (second getValCPSType) vs)
getValCPSType (TAppCPS _ t _ _) = t
getValCPSType (PackCPS _ _ _ t ) = t

substitute :: Id -> Type -> Type -> Type
substitute x new t = case t of
   I32 -> I32
   TVar y -> if x == y then new else t
   Forall y t' -> Forall y (substitute x new t')
   Arrow t1 t2 -> Arrow (substitute x new t1) (substitute x new t2)

typeEq :: Type -> Type -> Bool
typeEq I32 I32 = True
typeEq (TVar x) (TVar y) = x == y
typeEq (Forall x t1) (Forall y t2) = typeEq t1 $ substitute y (TVar x) t2
typeEq (Arrow t1 t2) (Arrow t3 t4) = typeEq t1 t3 && typeEq t2 t4
typeEq _ _ = False

prettyType :: Type -> String
prettyType I32 = "i32"
prettyType (TVar x) = "x" ++ show x
prettyType (Forall x t) = "∀x" ++ show x ++ ". " ++ prettyType t
prettyType (Arrow t1 t2) = "(" ++ prettyType t1 ++ " -> " ++ prettyType t2 ++ ")"

prettyExpr :: Expr -> String
prettyExpr (Lambda x t e) = "λx" ++ show x ++ ": " ++ prettyType t ++ ". " ++ prettyExpr e
prettyExpr (TLambda x e) = "Λx" ++ show x ++ ". " ++ prettyExpr e
prettyExpr (App _t e1 e2) = "(" ++ prettyExpr e1 ++ " " ++ prettyExpr e2 ++ ")"
prettyExpr (TApp _t e t') = prettyExpr e ++ "[" ++ prettyType t' ++ "]"
prettyExpr (Var x _t _global) = "x" ++ show x
prettyExpr (Lit x) = show x

prettyCPSType :: TypeCPS clos hoist alloc -> String
prettyCPSType I32CPS = "i32"
prettyCPSType (TVarCPS x) = "x" ++ show x
prettyCPSType (ArrowCPS xs ts) = if null xs then "(" ++ intercalate ", " (map prettyCPSType ts) ++ ")->0"
                                 else "∀" ++ intercalate ", " (map (("x"++).show) xs) ++ ". (" ++ intercalate ", " (map prettyCPSType ts) ++ ")->0"
prettyCPSType (ProductCPS ts) = if null ts then "()" else intercalate "*" (map (("("++).(++")").prettyCPSType.snd) ts)
prettyCPSType (ExistsCPS _ x t) = "∃x" ++ show x ++ ". " ++ prettyCPSType t

prettyCPSVal :: ValCPS clos hoist alloc -> String
prettyCPSVal (LitCPS x) = show x
prettyCPSVal (VarCPS x _t _global) = "x" ++ show x
prettyCPSVal (LambdaCPS _ tvars xs e) =
   if null tvars then
      "(λ" ++ intercalate ", " (map (\(x,t)->"x"++show x++": "++prettyCPSType t) xs) ++ ". " ++ prettyCPSExpr e ++ ")"
   else
      "(λ[" ++ intercalate ", " (map (("x"++).show) tvars) ++ "]" ++ intercalate ", " (map (\(x,t)->"x"++show x++": "++prettyCPSType t) xs) ++ ". " ++ prettyCPSExpr e ++ ")"
prettyCPSVal (TupleCPS _ xs) = "(" ++ intercalate ", " (map (prettyCPSVal . snd) xs) ++ ")"
prettyCPSVal (TAppCPS _ _ e ts) = prettyCPSVal e ++ "[" ++ intercalate ", " (map prettyCPSType ts) ++ "]"
prettyCPSVal (PackCPS _ t v t' ) = "pack[" ++ prettyCPSType t ++ ", " ++ prettyCPSVal v ++ "] as " ++ prettyCPSType t'

prettyCPSExpr :: ExprCPS clos hoist alloc -> String
prettyCPSExpr (AppCPS e ts xs) =
   if null ts then
      prettyCPSVal e ++ "(" ++ intercalate ", " (map prettyCPSVal xs) ++ ")"
   else
      prettyCPSVal e ++ "[" ++ intercalate ", " (map prettyCPSType ts) ++ "](" ++ intercalate ", " (map prettyCPSVal xs) ++ ")"
prettyCPSExpr (HaltCPS e) = "halt(" ++ prettyCPSVal e ++ ")"
prettyCPSExpr (LetCPS x t e1 e2) = "let x" ++ show x ++ ": " ++ prettyCPSType t ++ " = " ++ prettyCPSVal e1 ++ " in\n    " ++ prettyCPSExpr e2
prettyCPSExpr (TupleProjCPS x tpl i cont) = "let x" ++ show x ++ " = proj(" ++ show i ++ ", " ++ prettyCPSVal tpl ++ ") in\n    " ++ prettyCPSExpr cont
prettyCPSExpr (UnpackCPS _ x y v cont) = "let [x" ++ show x ++ ", x" ++ show y ++ "] = unpack(" ++ prettyCPSVal v ++ ") in\n    " ++ prettyCPSExpr cont
prettyCPSExpr (MallocCPS _ x ts cont) = "let x" ++ show x ++ " = malloc(" ++ intercalate ", " (map prettyCPSType ts) ++ ") in\n    " ++ prettyCPSExpr cont
prettyCPSExpr (InitCPS _ x tpl i v cont) = "let x" ++ show x ++ " = init(" ++ show i ++ ", " ++ prettyCPSVal tpl ++ ", " ++ prettyCPSVal v ++ ") in\n    " ++ prettyCPSExpr cont

data Stmt alloc = Func Id [Id] [(Id, TypeCPS U U alloc)] (ExprCPS U U alloc) deriving (Show, Eq)

prettyStmt :: Stmt alloc -> String
prettyStmt (Func x tvars params e) = "fn x" ++ show x ++ "["++intercalate ", " (map show tvars) ++ "](" ++ intercalate ", " (map (\(param,t)->"x"++show param++": "++prettyCPSType t) params) ++ ") {\n    " ++ prettyCPSExpr e ++ "\n}\n\n"

data Error = ParseError String
           | UnknownIdentifier String
           | TypeMismatch Type Type
           | CallingNonFunction Type
           | CallingNonForall Type
           deriving (Show, Eq)

data SLC_ a = Fine Id a | Fail Error deriving (Show, Eq)

newtype SLC a = SLC { runSLC :: Id -> SLC_ a}

instance Functor SLC where
   fmap :: (a -> b) -> SLC a -> SLC b
   fmap f (SLC g) = SLC $ \id ->
      case g id of
         Fine id2 x -> Fine id2 (f x)
         Fail e -> Fail e

instance Applicative SLC where
   pure :: a -> SLC a
   pure x = SLC $ flip Fine x
   (<*>) :: SLC (a -> b) -> SLC a -> SLC b
   (<*>) (SLC f) (SLC g) = SLC $ \id ->
      case f id of
         Fine id2 h -> case g id2 of
            Fine id3 x -> Fine id3 (h x)
            Fail e -> Fail e
         Fail e -> Fail e

instance Monad SLC where
   (>>=) :: SLC a -> (a -> SLC b) -> SLC b
   (>>=) (SLC f) g = SLC $ \id ->
         case f id of
            Fine id2 x -> runSLC (g x) id2
            Fail e -> Fail e
   return :: a -> SLC a
   return = pure

fresh :: SLC Id
fresh = SLC $ \id -> Fine (id + 1) id

fail :: Error -> SLC a
fail = SLC . const . Fail

run :: Id -> SLC a -> Either Error a
run id (SLC f) = case f id of
   Fine _ x -> Right x
   Fail e -> Left e