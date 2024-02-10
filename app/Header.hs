module Header where

data TypeSyntax = I32Syntax
                | TVarSyntax String
                | ForallSyntax String TypeSyntax
                | ArrowSyntax TypeSyntax TypeSyntax
                deriving (Show, Eq)

data ExprSyntax = LambdaSyntax String TypeSyntax ExprSyntax 
                | AppSyntax ExprSyntax ExprSyntax 
                | VarSyntax String 
                | LitSyntax Int 
                deriving (Show, Eq)