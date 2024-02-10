module Main where
import Parser

main :: IO ()
main = print (go " \\ x : i32 -> i32 . ( x ( 6 ) ) ")
