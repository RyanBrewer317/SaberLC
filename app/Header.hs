{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE InstanceSigs #-}

module Header where
import Prelude hiding (id)
import Data.List (intercalate)

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
          | Var Id Type
          | Lit Int
          deriving (Show, Eq)

data TypeCPS clos = I32CPS
                  | TVarCPS Id
                  | ArrowCPS [Id] [TypeCPS clos]
                  | ProductCPS [TypeCPS clos]
                  | ExistsCPS Id (TypeCPS clos) clos
                  deriving (Show, Eq)

data ValCPS clos = LitCPS Int
                 | VarCPS Id (TypeCPS clos)
                 | LambdaCPS [Id] [(Id, TypeCPS clos)] (ExprCPS clos)
                 | TupleCPS [ValCPS clos]
                 | TAppCPS (TypeCPS clos) (ValCPS clos) [TypeCPS clos] clos
                 | PackCPS (TypeCPS clos) (ValCPS clos) (TypeCPS clos) clos
                 deriving (Show, Eq)

data ExprCPS clos = AppCPS (ValCPS clos) [TypeCPS clos] [ValCPS clos]
                  | HaltCPS (ValCPS clos)
                  | LetCPS Id (TypeCPS clos) (ValCPS clos) (ExprCPS clos)
                  | TupleProjCPS Id (ValCPS clos) Int (ExprCPS clos)
                  | UnpackCPS Id Id (ValCPS clos) (ExprCPS clos) clos
                  deriving (Show, Eq)

getType :: Expr -> Type
getType (Lambda _ t e) = Arrow t (getType e)
getType (TLambda x e) = Forall x (getType e)
getType (App t _ _) = t
getType (TApp t _ _) = t
getType (Var _ t) = t
getType (Lit _) = I32

getTypeCPS :: ExprCPS clos -> TypeCPS clos
getTypeCPS (AppCPS {}) = undefined
getTypeCPS (HaltCPS v) = getValCPSType v
getTypeCPS (LetCPS _ _ _ e) = getTypeCPS e
getTypeCPS (TupleProjCPS _ _ _ e) = getTypeCPS e
getTypeCPS (UnpackCPS _ _ _ e _) = getTypeCPS e

getValCPSType :: ValCPS clos -> TypeCPS clos
getValCPSType (LitCPS _) = I32CPS
getValCPSType (VarCPS _ t) = t
getValCPSType (LambdaCPS tvars ts _) = ArrowCPS tvars (map snd ts)
getValCPSType (TupleCPS vs) = ProductCPS (map getValCPSType vs)
getValCPSType (TAppCPS t _ _ _) = t
getValCPSType (PackCPS _ _ t _) = t

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
prettyExpr (Var x _t) = "x" ++ show x
prettyExpr (Lit x) = show x

prettyCPSType :: TypeCPS clos -> String
prettyCPSType I32CPS = "i32"
prettyCPSType (TVarCPS x) = "x" ++ show x
prettyCPSType (ArrowCPS xs ts) = if null xs then "(" ++ intercalate ", " (map prettyCPSType ts) ++ ")->0"
                                 else "∀" ++ intercalate ", " (map (("x"++).show) xs) ++ ". (" ++ intercalate ", " (map prettyCPSType ts) ++ ")->0"
prettyCPSType (ProductCPS ts) = if null ts then "()" else intercalate "*" (map (("("++).(++")").prettyCPSType) ts)
prettyCPSType (ExistsCPS x t _) = "∃x" ++ show x ++ ". " ++ prettyCPSType t

prettyCPSVal :: ValCPS clos -> String
prettyCPSVal (LitCPS x) = show x
prettyCPSVal (VarCPS x _t) = "x" ++ show x
prettyCPSVal (LambdaCPS tvars xs e) =
   if null tvars then
      "(λ" ++ intercalate ", " (map (\(x,t)->"x"++show x++": "++prettyCPSType t) xs) ++ ". " ++ prettyCPSExpr e ++ ")"
   else
      "(λ[" ++ intercalate ", " (map (("x"++).show) tvars) ++ "]" ++ intercalate ", " (map (\(x,t)->"x"++show x++": "++prettyCPSType t) xs) ++ ". " ++ prettyCPSExpr e ++ ")"
prettyCPSVal (TupleCPS xs) = "(" ++ intercalate ", " (map prettyCPSVal xs) ++ ")"
prettyCPSVal (TAppCPS _ e ts _) = prettyCPSVal e ++ "[" ++ intercalate ", " (map prettyCPSType ts) ++ "]"
prettyCPSVal (PackCPS t v t' _) = "pack[" ++ prettyCPSType t ++ ", " ++ prettyCPSVal v ++ "] as " ++ prettyCPSType t'

prettyCPSExpr :: ExprCPS clos -> String
prettyCPSExpr (AppCPS e ts xs) =
   if null ts then
      prettyCPSVal e ++ "(" ++ intercalate ", " (map prettyCPSVal xs) ++ ")"
   else
      prettyCPSVal e ++ "[" ++ intercalate ", " (map prettyCPSType ts) ++ "](" ++ intercalate ", " (map prettyCPSVal xs) ++ ")"
prettyCPSExpr (HaltCPS e) = "halt(" ++ prettyCPSVal e ++ ")"
prettyCPSExpr (LetCPS x t e1 e2) = "let x" ++ show x ++ ": " ++ prettyCPSType t ++ " = " ++ prettyCPSVal e1 ++ " in\n    " ++ prettyCPSExpr e2
prettyCPSExpr (TupleProjCPS x tpl i cont) = "let x" ++ show x ++ " = proj(" ++ show i ++ ", " ++ prettyCPSVal tpl ++ ") in\n    " ++ prettyCPSExpr cont
prettyCPSExpr (UnpackCPS x y v cont _) = "let [x" ++ show x ++ ", x" ++ show y ++ "] = unpack(" ++ prettyCPSVal v ++ ") in\n    " ++ prettyCPSExpr cont

data Stmt = Func Id [Id] [(Id, TypeCPS ())] (ExprCPS ()) deriving (Show, Eq)

prettyStmt :: Stmt -> String
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