{
module Lexer(Token(..), lexer) where 

import LexerUtils
import LexerDimension
}

%wrapper "basic"

$digit      = [0-9]
$hexdig     = [0-9A-Fa-f]
$nonascii   = []

$char       = [0-9a-zA-Z\-_]
$firstChar  = [_a-zA-Z] 
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

@ident      = \-? $firstChar @nmchar*
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
    @comment            { Comment }
    @badComment         { BadComment }
    @badString          { BadStringTok }

    @ident              { Ident }
    \@ @ident           { AtKeyword . tail }

    \# @name            { Hash . tail }

    @num                { Number . read }
    @num \%             { Percentage . readNum . init }
    @num @ident         { uncurry Dimension . readDim }

    @string             { StringTok . stripQuotes }

    @urlName "(" @w @string @w ")"    { Uri . stripQuotes . stripUri }
    @urlName "(" @w @url    @w ")"    { Uri . stripUri }

    : 	                { const Colon }
    \;                  { const SemiColon } 
    \{                  { const OpenCurly }
    \}                  { const CloseCurly }
    \(                  { const OpenParen }
    \)                  { const CloseParen }
    \[                  { const OpenSquare }
    \]                  { const CloseSquare }
    \,                  { const Comma }
    \/                  { const Slash }
    \.                  { const Period }
    !                   { const Bang }

    @ident \(           { Function . init }

    "~="                { const Includes }
    "|="                { const DashMatch }    
    "^="                { const PrefixMatch	}
    "$="                { const SuffixMatch }
    "*="                { const SubstringMatch }
    "="                 { const EqualsMatch	}
    "*"                 { const Mult }
    "+"                 { const Plus }
    "~"                 { const Tilde }
    ">"                 { const Child }

    . # [\"\']          { Delim . head }

{
data Token 
    = Ident String
    | Comment String
    | BadComment String
    | AtKeyword String
    | StringTok String
    | BadStringTok String
    | Hash String
    | Number Double
    | Percentage Double
    | Dimension Double String
    | Uri String
    | UnicodeRange String
    | Colon
    | SemiColon
    | Comma
    | Slash
    | Period
    | Bang
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

lexer :: String -> [Token]
lexer = alexScanTokens
}
