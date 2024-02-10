{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

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