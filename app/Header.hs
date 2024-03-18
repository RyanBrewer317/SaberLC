{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}
module Header where
import Data.List (intercalate)

class Pretty a where
    pretty :: a -> String

data TypeParse
    = I32Parse
    | TypeVarParse String
    | ForallParse String TypeParse
    | FunctionTypeParse TypeParse TypeParse

instance Pretty TypeParse where
    pretty typeParse = case typeParse of
        I32Parse -> "i32"
        TypeVarParse name -> name
        ForallParse name body -> "forall " ++ name ++ ". " ++ pretty body
        FunctionTypeParse argTypeParse returnTypeParse -> pretty argTypeParse ++ " -> " ++ pretty returnTypeParse

data ExprParse
    = VarParse Bool String
    | IntLitParse Int
    | LambdaParse String TypeParse ExprParse
    | AppParse ExprParse ExprParse
    | TypeLambdaParse String ExprParse
    | TypeAppParse ExprParse TypeParse

instance Pretty ExprParse where
    pretty exprParse = case exprParse of
        VarParse _ name -> name
        IntLitParse int -> show int
        LambdaParse name typeParse body -> "\\" ++ name ++ ": " ++ pretty typeParse ++ ". " ++ pretty body
        AppParse f x -> "(" ++ pretty f ++ ")(" ++ pretty x ++ ")"
        TypeLambdaParse name body -> "/\\ " ++ name ++ ". " ++ pretty body
        TypeAppParse expr typeParse -> "(" ++ pretty expr ++ ")[" ++ pretty typeParse ++ "]"

data Ident 
    = Local Int 
    | Global String 
    deriving (Eq)

instance Pretty Ident where
    pretty ident = case ident of
        Local num -> "x" ++ show num
        Global name -> name

data TypeTC
    = I32TC
    | TypeVarTC Ident
    | ForallTC Int TypeTC
    | FunctionTypeTC TypeTC TypeTC
    deriving (Eq)

eqTC :: TypeTC -> TypeTC -> Bool
eqTC typeTC1 typeTC2 = case (typeTC1, typeTC2) of
    (I32TC, I32TC) -> True
    (TypeVarTC idNum1, TypeVarTC idNum2) -> idNum1 == idNum2
    (ForallTC idNum1 body1, ForallTC idNum2 body2) -> eqTC body1 $ substitute idNum2 (TypeVarTC $ Local idNum1) body2
    (FunctionTypeTC argType1 resultType1, FunctionTypeTC argType2 resultType2) -> eqTC argType1 argType2 && eqTC resultType1 resultType2
    (_, _) -> False

substitute :: Int -> TypeTC -> TypeTC -> TypeTC
substitute idNum newType t = case t of
    TypeVarTC (Local idNum2) -> if idNum == idNum2 then newType else TypeVarTC $ Local idNum2
    TypeVarTC ident -> TypeVarTC ident
    I32TC -> I32TC
    ForallTC paramIdNum bodyTC -> ForallTC paramIdNum (substitute idNum newType bodyTC)
    FunctionTypeTC paramType resultType -> FunctionTypeTC (substitute idNum newType paramType) (substitute idNum newType resultType)


instance Pretty TypeTC where
    pretty typeTC = case typeTC of
        I32TC -> "i32"
        TypeVarTC ident -> pretty ident
        ForallTC idNum body -> "forall " ++ "t" ++ show idNum ++ ". " ++ pretty body
        FunctionTypeTC argTypeTC returnTypeTC -> pretty argTypeTC ++ " -> " ++ pretty returnTypeTC

data ExprTC
    = VarTC TypeTC Bool Ident
    | IntLitTC Int
    | LambdaTC Int TypeTC ExprTC
    | AppTC TypeTC ExprTC ExprTC
    | TypeLambdaTC Int ExprTC
    | TypeAppTC TypeTC ExprTC TypeTC

instance Pretty ExprTC where
    pretty exprTC = case exprTC of
        VarTC _ _ ident -> pretty ident
        IntLitTC int -> show int
        LambdaTC var typeTC body -> "\\" ++ "x" ++ show var ++ ": " ++ pretty typeTC ++ ". " ++ pretty body
        AppTC _ f x -> "(" ++ pretty f ++ ")(" ++ pretty x ++ ")"
        TypeLambdaTC var body -> "/\\ " ++ "x" ++ show var ++ ". " ++ pretty body
        TypeAppTC _ expr typeTC -> "(" ++ pretty expr ++ ")[" ++ pretty typeTC ++ "]"

typeOfTC :: ExprTC -> TypeTC
typeOfTC exprTC = case exprTC of
    VarTC typeTC _ _ -> typeTC
    IntLitTC _ -> I32TC
    LambdaTC _ typeTC body -> FunctionTypeTC typeTC (typeOfTC body)
    AppTC typeTC _ _ -> typeTC
    TypeLambdaTC var body -> ForallTC var (typeOfTC body)
    TypeAppTC typeTC _ _ -> typeTC

data TypeCPS
    = I32CPS
    | TypeVarCPS Ident
    | ForallCPS Int TypeCPS
    | FunctionTypeCPS [TypeCPS]
    deriving Eq

instance Pretty TypeCPS where
    pretty typeCPS = case typeCPS of
        I32CPS -> "i32"
        TypeVarCPS ident -> pretty ident
        ForallCPS idNum body -> "forall x" ++ show idNum ++ ". " ++ pretty body
        FunctionTypeCPS argTypesCPS -> "(" ++ intercalate ", " (map pretty argTypesCPS) ++ ")->0"

data ValCPS
    = VarCPS TypeCPS Bool Ident
    | IntLitCPS Int
    | LambdaCPS [(Int, TypeCPS)] ExprCPS
    | TypeLambdaCPS [Int] ValCPS
    | TypeAppCPS TypeCPS ValCPS [TypeCPS]

instance Pretty ValCPS where
    pretty valCPS = case valCPS of
        VarCPS _ _ ident -> pretty ident
        IntLitCPS int -> show int
        LambdaCPS params body -> "\\" ++ intercalate ", " (map (\(idNum, typeCPS) -> "x" ++ show idNum ++ ": " ++ pretty typeCPS) params) ++ ". " ++ pretty body
        TypeLambdaCPS params body -> "/\\ " ++ intercalate ", " (map (("x"++).show) params) ++ ". " ++ pretty body
        TypeAppCPS _ expr typeCPS -> "(" ++ pretty expr ++ ")[" ++ intercalate ", " (map pretty typeCPS) ++ "]"

data ExprCPS
    = HaltCPS ValCPS
    | AppCPS ValCPS [ValCPS]

instance Pretty ExprCPS where
    pretty exprCPS = case exprCPS of
        HaltCPS v -> "halt " ++ pretty v
        AppCPS valCPS argsCPS -> "(" ++ pretty valCPS ++ ")(" ++ intercalate ", " (map pretty argsCPS) ++ ")"

data Error
  = ParseError String
  | UnknownIdentifier String
  | TypeError TypeTC TypeTC
  | CallingNonFunction TypeTC
  | CallingNonForall TypeTC

instance Pretty Error where
    pretty err = case err of
        ParseError msg -> "Parse error: " ++ msg
        UnknownIdentifier name -> "Unknown identifier: " ++ name
        TypeError expected actual -> "Type error: expected " ++ pretty expected ++ ", got " ++ pretty actual
        CallingNonFunction typeTC -> "Calling non-function: " ++ pretty typeTC
        CallingNonForall typeTC -> "Calling non-forall: " ++ pretty typeTC

data SLC_ a = Fine Int a | Fail Error

newtype SLC a = SLC {runSLC :: Int -> SLC_ a}

instance Functor SLC where
  fmap f (SLC g) = SLC $ \nameGen ->
    case g nameGen of
      Fine nameGen2 x -> Fine nameGen2 (f x)
      Fail e -> Fail e

instance Applicative SLC where
  pure x = SLC $ flip Fine x
  (<*>) (SLC f) (SLC g) = SLC $ \nameGen ->
    case f nameGen of
      Fine nameGen2 h -> case g nameGen2 of
        Fine nameGen3 x -> Fine nameGen3 (h x)
        Fail e -> Fail e
      Fail e -> Fail e

instance Monad SLC where
  (>>=) (SLC f) g = SLC $ \nameGen ->
    case f nameGen of
      Fine id2 x -> runSLC (g x) id2
      Fail e -> Fail e
  return = pure

fresh :: SLC Int
fresh = SLC $ \nameGen -> Fine (nameGen + 1) nameGen

throw :: Error -> SLC a
throw = SLC . const . Fail

run :: Int -> SLC a -> Either Error a
run nameGen (SLC f) = case f nameGen of
  Fine _ x -> Right x
  Fail e -> Left e
