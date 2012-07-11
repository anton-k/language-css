{
module LexerDimension(readDim) where

import LexerUtils
}

%wrapper "basic"

$digit      = [0-9]
$sign       = [\+\-]

$char       = [a-zA-Z\-_]

@num        = $sign? ($digit+ | $digit* \. $digit+ )
@dim        = $char+

token :-
    @num        { Dnum . readNum }
    @dim        { Ddim }

{
data Dim = Dnum Double | Ddim String

readDim :: String -> (Double, String)
readDim = phi . alexScanTokens 
    where phi (Dnum a : Ddim b : _) = (a, b)
}
