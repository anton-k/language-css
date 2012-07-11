module LexerUtils(
    readNum, stripQuotes, stripUri) where

readNum :: String -> Double
readNum x = read $ case x of
    '.' : as        -> zero as
    '-' : '.' : as  -> '-' : zero as
    '+' : '.' : as  -> zero as
    _               -> x
    where zero as = '0' : '.' : as

stripQuotes :: String -> String
stripQuotes = init . tail

stripUri :: String -> String 
stripUri = init . drop 4

