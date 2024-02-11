{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}


module Main where
import qualified Parser
import qualified AST
import qualified CPS
import qualified ClosureConv
import qualified Hoist
import Header
import Control.Monad ((>=>))
import Data.Map

main :: IO ()
main = case run 0 $ go "(\\x: i32. x)(3)" of
   Left err -> print err
   Right stmts -> putStrLn $ concatMap prettyStmt stmts

go :: String -> SLC [Stmt]
go = Parser.go >=> AST.go empty empty >=> CPS.go >=> ClosureConv.go >=> Hoist.go
