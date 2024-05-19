{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}
module Main where

import Header
import qualified Parser
import qualified Typechecker
import qualified CPS
import qualified ClosureConversion
import qualified Hoist
import qualified Alloc
import qualified Regions
import qualified Codegen
import Control.Monad ((>=>))
import qualified Data.ByteString as BIN
import System.IO
import Data.Binary (Word8)

main :: IO ()
main = case run 0 $ go "(\\x: i32. x)(3)" of
    Left err -> putStrLn $ pretty err
    Right bytes -> do
        h_out <- openFile "bin.svm" WriteMode
        BIN.hPut h_out $ BIN.pack bytes
        hClose h_out

go :: String -> SLC [Word8]
go = Parser.go >=> Typechecker.go >=> CPS.go >=> ClosureConversion.go >=> Hoist.go >=> Alloc.go >=> Regions.go >=> Codegen.go
