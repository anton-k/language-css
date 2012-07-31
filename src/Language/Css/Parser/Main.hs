module Main where

import Language.Css.Pretty

import Lexer
import Parser

pl p = parser p . lexer

main' = print . parser styleSheet . lexer =<< readFile "ocean.css"

main = writeFile "out.css" . show . pretty . fromRight 
    . parser styleSheet . lexer =<< readFile "ocean.css"

fromRight = either undefined id

test x = mapM_ print . lexer =<< readFile x

isDelim x = case x of
    Delim a -> True
    _       -> False
