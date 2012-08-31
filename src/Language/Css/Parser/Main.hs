module Main where

import Language.Css.Pretty

import Lexer
import Parser

parse' = fromRight . parser styleSheet . lexer
--pretty' = show . pretty

pl p = parser p . lexer

main = writeFile "dump.css" . show . pretty . fromRight . pl styleSheet =<< readFile "css-examples/wk.css"

main' = writeFile "out.css" . show . pretty . fromRight 
    . parser styleSheet . lexer =<< readFile "css-examples/wk.css"

fromRight = either undefined id

test x = mapM_ print . lexer =<< readFile x

isDelim x = case x of
    Delim a -> True
    _       -> False
