{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}
module Header where

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
    = VarParse String
    | IntLitParse Int
    | LambdaParse String TypeParse ExprParse
    | AppParse ExprParse ExprParse
    | TypeLambdaParse String ExprParse
    | TypeAppParse ExprParse TypeParse

instance Pretty ExprParse where
    pretty exprParse = case exprParse of
        VarParse name -> name
        IntLitParse int -> show int
        LambdaParse name typeParse body -> "\\" ++ name ++ ": " ++ pretty typeParse ++ ". " ++ pretty body
        AppParse f x -> pretty f ++ "(" ++ pretty x ++ ")"
        TypeLambdaParse name body -> "/\\ " ++ name ++ ". " ++ pretty body
        TypeAppParse expr typeParse -> pretty expr ++ "[" ++ pretty typeParse ++ "]"

type Id = Int

data TypeTC
    = I32TC
    | TypeVarTC Id
    | ForallTC Id TypeTC
    | FunctionTypeTC TypeTC TypeTC

instance Pretty TypeTC where
    pretty typeTC = case typeTC of
        I32TC -> "i32"
        TypeVarTC idNum -> "t" ++ show idNum
        ForallTC idNum body -> "forall " ++ "t" ++ show idNum ++ ". " ++ pretty body
        FunctionTypeTC argTypeTC returnTypeTC -> pretty argTypeTC ++ " -> " ++ pretty returnTypeTC

data Error
  = ParseError String
  | UnknownIdentifier String

instance Pretty Error where
    pretty err = case err of
        ParseError msg -> "Parse error: " ++ msg
        UnknownIdentifier name -> "Unknown identifier: " ++ name

data SLC_ a = Fine Id a | Fail Error

newtype SLC a = SLC {runSLC :: Id -> SLC_ a}

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

fresh :: SLC Id
fresh = SLC $ \nameGen -> Fine (nameGen + 1) nameGen

throw :: Error -> SLC a
throw = SLC . const . Fail

run :: Id -> SLC a -> Either Error a
run nameGen (SLC f) = case f nameGen of
  Fine _ x -> Right x
  Fail e -> Left e
