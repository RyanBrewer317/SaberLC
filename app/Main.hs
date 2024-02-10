{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}


module Main where
import qualified Parser
import qualified AST
import Header
import Control.Monad ((>=>))
import Data.Map

main :: IO ()
main = case run 0 $ go " \\ id : forall a. a->a . id[forall b. b->b](id)[i32](id[i32](4)) " of
   Left err -> print err
   Right e -> putStrLn $ prettyExpr e

go :: String -> SLC Expr
go = Parser.go >=> AST.go empty empty
