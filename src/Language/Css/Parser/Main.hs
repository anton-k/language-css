module Main where

import Lexer

main = return () --  mapM_ print . lexer =<< readFile "ocean.css"

test x = mapM_ print . lexer =<< readFile x

isDelim x = case x of
    Delim a -> True
    _       -> False
