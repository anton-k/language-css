module Token where

data L a = L Pos a
    deriving (Show, Eq)

-- (line, column)
type Pos = (Int, Int)

data Token 
    = Ident String
    | VendorPrefix String
    | Comment String
    | BadComment String

    | Important

    | AtRule (Maybe String) String

    | StringTok String
    | BadStringTok String
    | Hash String
    | Number Double
    | Percent Double
    | Dimension Double String
    | Uri String
    | UnicodeRange String
    | Colon
    | SemiColon
    | Comma
    | Slash
    | Period
    | Bang
    | Greater
    | Bar
    | OpenParen
    | CloseParen
    | OpenCurly
    | CloseCurly
    | OpenSquare
    | CloseSquare
    | Function String 
    | Includes
    | DashMatch
    | PrefixMatch
    | SuffixMatch 
    | SubstringMatch 
    | EqualsMatch
    | Mult 
    | Plus 
    | Tilde 
    | Child 
    | Delim Char
    deriving (Show, Eq)      




