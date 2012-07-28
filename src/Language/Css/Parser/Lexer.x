{
module Lexer(Token(..), lexer) where 

import LexerUtils
import LexerDimension

import Token
}

%wrapper "posn"

$digit      = [0-9]
$hexdig     = [0-9A-Fa-f]
$nonascii   = []

$char       = [0-9a-zA-Z\-_]
$firstChar  = [a-zA-Z] 
$sign       = [\+\-]

@unicode    = \\ $hexdig{1,6} $white?

@nmchar     = $char | $nonascii | @unicode 

@lineterm   = [\n\r\f] | \r\n
@any        = . | @lineterm

$butStar    = . # \*
$butSlash   = . # \/
$butMinus   = . # \-
$butGt      = . # >

@anyComm1   = @lineterm | $butStar | \* $butSlash
@anyComm2   = @lineterm | $butMinus | \- $butMinus | "--" $butGt 

@comm1      = "/*"   @anyComm1* "*/"
@comm2      = "<!--" @anyComm2* "-->"

@comment    = @comm1 | @comm2

@badComm1   = "/*"   @anyComm1* 
@badComm2   = "<!--" @anyComm2* 

@badComment = @badComm1 | @badComm2

@vendor     = [\-_] $firstChar* \-
@ident      = $firstChar @nmchar*
@name       = $char+


@num        = $sign? ($digit+ | $digit* \. $digit+)


@escape		= @unicode | \\[^\r\n\f0-9a-f]

@str1       = \" (@escape | \\ @lineterm | ~[\"])* \"
@str2       = '  (@escape | \\ @lineterm | ~['])*  '

@string     = @str1 | @str2

@badStr1    = \" (@escape | \\ @lineterm | ~[\"])* 
@badStr2    = '  (@escape | \\ @lineterm | ~['])* 

@badString  = @badStr1 | @badStr2

@url        = (@escape | \\ @lineterm | ~[\"\)\('])* 

-- case insensitive name for 'url'
$u'         = [uU] 
$r'         = [rR]
$l'         = [lL]

@urlName    = $u' $r' $l'

@w          = $white*

token :-

    $white+             ;
    @comment            { con Comment }
    @badComment         { con BadComment }
    @badString          { con BadStringTok }

    @vendor             { con VendorPrefix }     
    @ident              { con Ident }

    "!important"        { tok Important }

    "@" @vendor @ident  { con $ (\(a, b) -> AtRule (Just a) b) . stripAtRule . tail } 
    "@" @ident          { con $ AtRule Nothing . tail }

    \# @name            { con $ Hash . tail }

    @num                { con $ Number . read }
    @num \%             { con $ Percentage . readNum . init }
    @num @ident         { con $ uncurry Dimension . readDim }

    @string             { con $ StringTok . stripQuotes }

    @urlName "(" @w @string @w ")"    { con $ Uri . stripQuotes . stripUri }
    @urlName "(" @w @url    @w ")"    { con $ Uri . stripUri }

    : 	                { tok Colon }
    \;                  { tok SemiColon } 
    \{                  { tok OpenCurly }
    \}                  { tok CloseCurly }
    \(                  { tok OpenParen }
    \)                  { tok CloseParen }
    \[                  { tok OpenSquare }
    \]                  { tok CloseSquare }
    \,                  { tok Comma }
    \/                  { tok Slash }
    \.                  { tok Period }
    !                   { tok Bang }

    @ident \(           { con $ Function . init }

    "~="                { tok Includes }
    "|="                { tok DashMatch }    
    "^="                { tok PrefixMatch	}
    "$="                { tok SuffixMatch }
    "*="                { tok SubstringMatch }
    "="                 { tok EqualsMatch	}
    "*"                 { tok Mult }
    "+"                 { tok Plus }
    "~"                 { tok Tilde }
    ">"                 { tok Child }

    . # [\"\']          { con $ Delim . head }

{
pos :: AlexPosn -> Pos
pos (AlexPn _ l c) = (l,c)

type TokenFun = AlexPosn -> String -> L Token

-- primitive tokens
tok :: Token -> TokenFun 
tok t p _ = L (pos p) t 

-- constructors
con :: (String -> Token) -> TokenFun
con f p s = L (pos p) (f s)

lexer :: String -> [L Token]
lexer = alexScanTokens
}
