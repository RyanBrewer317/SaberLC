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
    | TypeLambdaCPS Int ValCPS
    | TypeAppCPS TypeCPS ValCPS TypeCPS

instance Pretty ValCPS where
    pretty valCPS = case valCPS of
        VarCPS _ _ ident -> pretty ident
        IntLitCPS int -> show int
        LambdaCPS params body -> "\\" ++ intercalate ", " (map (\(idNum, typeCPS) -> "x" ++ show idNum ++ ": " ++ pretty typeCPS) params) ++ ". " ++ pretty body
        TypeLambdaCPS param body -> "/\\ x" ++ show param ++ ". " ++ pretty body
        TypeAppCPS _ expr t -> "(" ++ pretty expr ++ ")[" ++ pretty t ++ "]"

data ExprCPS
    = HaltCPS ValCPS
    | AppCPS ValCPS [ValCPS]

instance Pretty ExprCPS where
    pretty exprCPS = case exprCPS of
        HaltCPS v -> "halt " ++ pretty v
        AppCPS valCPS argsCPS -> "(" ++ pretty valCPS ++ ")(" ++ intercalate ", " (map pretty argsCPS) ++ ")"

data TypeCC
    = TypeVarCC Ident
    | I32CC
    | ForallCC [Int] TypeCC
    | FunctionTypeCC [TypeCC]
    | TupleTypeCC [TypeCC]
    | ExistialCC Int TypeCC

instance Pretty TypeCC where
    pretty typeCC = case typeCC of
        TypeVarCC ident -> pretty ident
        I32CC -> "i32"
        ForallCC params body -> "forall " ++ intercalate ", " (map (("x"++).show) params) ++ ". " ++ pretty body
        FunctionTypeCC argTypesCC -> "(" ++ intercalate ", " (map pretty argTypesCC) ++ ")->0"
        TupleTypeCC argTypesCC -> "(" ++ intercalate ", " (map pretty argTypesCC) ++ ")"
        ExistialCC idNum body -> "some x" ++ show idNum ++ ". " ++ pretty body

data ValCC
    = VarCC TypeCC Bool Ident
    | IntLitCC Int
    | LambdaCC [(Int, TypeCC)] ExprCC
    | TypeLambdaCC [Int] ValCC
    | TypeAppCC TypeCC ValCC [TypeCC]
    | TupleCC [ValCC]
    | PackCC TypeCC ValCC TypeCC

instance Pretty ValCC where
    pretty valCC = case valCC of
        VarCC _ _ ident -> pretty ident
        IntLitCC int -> show int
        LambdaCC params body -> "\\" ++ intercalate ", " (map (\(idNum, typeCC) -> "x" ++ show idNum ++ ": " ++ pretty typeCC) params) ++ ". " ++ pretty body
        TypeLambdaCC params body -> "/\\" ++ intercalate ", " (map (("x"++).show) params) ++ ". " ++ pretty body
        TypeAppCC _ expr typeCC -> "(" ++ pretty expr ++ ")[" ++ intercalate ", " (map pretty typeCC) ++ "]"
        TupleCC valsCC -> "(" ++ intercalate ", " (map pretty valsCC) ++ ")"
        PackCC type1 val type2 -> "pack " ++ pretty val ++ " hiding " ++ pretty type1 ++ " as " ++ pretty type2

typeOfCPSVal :: ValCPS -> TypeCPS
typeOfCPSVal valCPS = case valCPS of
    VarCPS typeCPS _ _ -> typeCPS
    IntLitCPS _ -> I32CPS
    LambdaCPS params _ -> FunctionTypeCPS (map snd params)
    TypeLambdaCPS param body -> ForallCPS param (typeOfCPSVal body)
    TypeAppCPS typeCPS _ _ -> typeCPS

data ExprCC
    = HaltCC ValCC
    | AppCC ValCC [ValCC]
    | ProjCC Int TypeCC ValCC Int ExprCC
    | UnpackCC Int Int ValCC ExprCC

instance Pretty ExprCC where
    pretty exprCC = case exprCC of
        HaltCC v -> "halt " ++ pretty v
        AppCC valCC argsCC -> "(" ++ pretty valCC ++ ")(" ++ intercalate ", " (map pretty argsCC) ++ ")"
        ProjCC idNum typeCC valCC i scope -> "let x" ++ show idNum ++ ": " ++ pretty typeCC ++ " = " ++ pretty valCC ++ "[" ++ show i ++ "] in " ++ pretty scope
        UnpackCC idNum1 idNum2 valCC scope -> "let [x" ++ show idNum1 ++ ", x" ++ show idNum2 ++ "] = unpack(" ++ pretty valCC ++ ") in " ++ pretty scope

data TypeH 
    = TypeVarH Ident
    | I32H
    | ForallH [Int] TypeH
    | FunctionTypeH [TypeH]
    | TupleTypeH [TypeH]
    | ExistentialH Int TypeH

instance Pretty TypeH where
    pretty typeH = case typeH of
        TypeVarH ident -> pretty ident
        I32H -> "i32"
        ForallH params body -> "forall " ++ intercalate ", " (map (("x"++).show) params) ++ ". " ++ pretty body
        FunctionTypeH argTypesH -> "(" ++ intercalate ", " (map pretty argTypesH) ++ ")->0"
        TupleTypeH argTypesH -> "(" ++ intercalate ", " (map pretty argTypesH) ++ ")"
        ExistentialH idNum body -> "some x" ++ show idNum ++ ". " ++ pretty body

data ValH
    = VarH TypeH Bool Ident
    | IntLitH Int
    | TypeLambdaH [Int] ValH
    | TypeAppH TypeH ValH [TypeH]
    | TupleH [ValH]
    | PackH TypeH ValH TypeH

instance Pretty ValH where
    pretty valH = case valH of
        VarH _ _ ident -> pretty ident
        IntLitH int -> show int
        TypeLambdaH params body -> "/\\" ++ intercalate ", " (map (("x"++).show) params) ++ ". " ++ pretty body
        TypeAppH _ expr typeHs -> "(" ++ pretty expr ++ ")[" ++ intercalate ", " (map pretty typeHs) ++ "]"
        TupleH valsH -> "(" ++ intercalate ", " (map pretty valsH) ++ ")"
        PackH type1 val type2 -> "pack " ++ pretty val ++ " hiding " ++ pretty type1 ++ " as " ++ pretty type2

data ExprH
    = HaltH ValH
    | AppH ValH [ValH]
    | ProjH Int TypeH ValH Int ExprH
    | UnpackH Int Int ValH ExprH

instance Pretty ExprH where
    pretty expr = case expr of
        HaltH v -> "    halt " ++ pretty v
        AppH f args -> "    " ++ pretty f ++ "(" ++ intercalate ", " (map pretty args) ++ ")"
        ProjH idNum t tpl i scope -> "    let x" ++ show idNum ++ ": " ++ pretty t ++ " = " ++ pretty tpl ++ "[" ++ show i ++ "]\n" ++ pretty scope
        UnpackH idNum1 idNum2 v scope -> "    let [x" ++ show idNum1 ++ ", x" ++ show idNum2 ++ "] = unpack(" ++ pretty v ++ ")\n" ++ pretty scope

data StmtH
    = FuncH Int [Int] [(Int, TypeH)] ExprH

instance Pretty StmtH where
    pretty stmt = case stmt of
        FuncH idNum tvars params body -> "fn x" ++ show idNum ++ "[" ++ intercalate ", " (map (("x"++).show) tvars) ++ "](" ++ intercalate ", " (map (\(x, typeH) -> "x" ++ show x ++ ": " ++ pretty typeH) params) ++ ") {\n" ++ pretty body ++ "\n}"

data TypeA
    = TypeVarA Ident
    | I32A
    | ForallA [Int] TypeA
    | FunctionTypeA [TypeA]
    | TupleTypeA [TypeA]
    | ExistentialA Int TypeA

instance Pretty TypeA where
    pretty t = case t of
        TypeVarA ident -> pretty ident
        I32A -> "i32"
        ForallA params body -> "forall " ++ intercalate ", " (map (("x"++).show) params) ++ ". " ++ pretty body
        FunctionTypeA argTypesH -> "(" ++ intercalate ", " (map pretty argTypesH) ++ ")->0"
        TupleTypeA argTypesH -> "(" ++ intercalate ", " (map pretty argTypesH) ++ ")"
        ExistentialA idNum body -> "some x" ++ show idNum ++ ". " ++ pretty body

data ValA
    = VarA TypeA Bool Ident
    | IntLitA Int
    | TypeLambdaA [Int] ValA
    | TypeAppA TypeA ValA [TypeA]
    | TupleA [ValA]
    | PackA TypeA ValA TypeA

instance Pretty ValA where
    pretty v = case v of
        VarA _ _ ident -> pretty ident
        IntLitA int -> show int
        TypeLambdaA params body -> "/\\" ++ intercalate ", " (map (("x"++).show) params) ++ ". " ++ pretty body
        TypeAppA _ expr typeHs -> "(" ++ pretty expr ++ ")[" ++ intercalate ", " (map pretty typeHs) ++ "]"
        TupleA valsH -> "(" ++ intercalate ", " (map pretty valsH) ++ ")"
        PackA type1 val type2 -> "pack " ++ pretty val ++ " hiding " ++ pretty type1 ++ " as " ++ pretty type2

data ExprA
    = HaltA ValA
    | AppA ValA [ValA]
    | ProjA Int TypeA ValA Int ExprA
    | UnpackA Int Int ValA ExprA
    | MallocA Int [TypeA] ExprA
    | InitA Int ValA Int ValA ExprA

instance Pretty ExprA where
    pretty expr = case expr of
        HaltA v -> "    halt " ++ pretty v
        AppA f args -> "    " ++ pretty f ++ "(" ++ intercalate ", " (map pretty args) ++ ")"
        ProjA idNum t tpl i scope -> "    let x" ++ show idNum ++ ": " ++ pretty t ++ " = " ++ pretty tpl ++ "[" ++ show i ++ "]\n" ++ pretty scope
        UnpackA idNum1 idNum2 v scope -> "    let [x" ++ show idNum1 ++ ", x" ++ show idNum2 ++ "] = unpack(" ++ pretty v ++ ")\n" ++ pretty scope
        MallocA idNum types scope -> "    let x" ++ show idNum ++ ": (" ++ intercalate ", " (map pretty types) ++ ") = malloc (" ++ intercalate ", " (map pretty types) ++ ")\n" ++ pretty scope
        InitA idNum tpl i v scope -> "    let x" ++ show idNum ++ " = " ++ pretty tpl ++ "[" ++ show i ++ "] <- " ++ pretty v ++ "\n" ++ pretty scope

data StmtA
    = FuncA Int [Int] [(Int, TypeA)] ExprA

instance Pretty StmtA where
    pretty stmt = case stmt of
        FuncA idNum tvars params body -> "fn x" ++ show idNum ++ "[" ++ intercalate ", " (map (("x"++).show) tvars) ++ "](" ++ intercalate ", " (map (\(x, typeH) -> "x" ++ show x ++ ": " ++ pretty typeH) params) ++ ") {\n" ++ pretty body ++ "\n}"


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
