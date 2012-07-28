module Main where

import Lexer

main = mapM_ print . lexer =<< readFile "test.css"

test x = mapM_ print . lexer =<< readFile x

isDelim x = case x of
    Delim a -> True
    _       -> False
