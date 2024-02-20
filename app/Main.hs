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
import qualified Alloc
import qualified Codegen
import Header
import Control.Monad ((>=>))
import Data.Map
import Data.Word (Word8)
import qualified Data.ByteString as BIN
import System.IO

main :: IO ()
main = case run 0 $ go "(\\x: i32. x)(3)" of
   Left err -> print err
   Right bytes -> do
       h_out <- openFile "bin.svm" WriteMode
       BIN.hPut h_out $ BIN.pack bytes
       hClose h_out

go :: String -> SLC [Word8]
go = Parser.go >=> AST.go empty empty >=> CPS.go >=> ClosureConv.go >=> Hoist.go >=> Alloc.go >=> Codegen.go
