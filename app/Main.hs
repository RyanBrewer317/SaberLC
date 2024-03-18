{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}
module Main where

import Header
import qualified Parser

main :: IO ()
main = case run 0 $ go "(\\x: i32. x)(3)" of
    Left err -> putStrLn $ prettyError err
    Right e -> putStrLn $ prettyExprParse e
--  Right bytes -> do
--      h_out <- openFile "bin.svm" WriteMode
--      BIN.hPut h_out $ BIN.pack bytes
--      hClose h_out

go :: String -> SLC ExprParse
go = Parser.go
