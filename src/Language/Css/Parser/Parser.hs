module Parser where

import Control.Applicative

import Data.Char(toLower)

import Text.ParserCombinators.Parsec 
    hiding (many, (<|>), string)

import Text.ParserCombinators.Parsec.Pos

import Language.Css.Syntax
import qualified Language.Css.Syntax as S

import Token
import qualified Token as T

type P = GenParser (L Token) ()

lol = undefined

styleSheet :: P StyleSheet
styleSheet = StyleSheet <$> many rule


rule :: P Rule
rule =  SRuleSet <$> ruleSet 
    <|> uncurry SAtRule <$> atRule 

ruleSet :: P RuleSet
ruleSet = RuleSet <$> groupSel <*> decls

groupSel :: P GroupSel
groupSel = lol

decls :: P [Decl]
decls = braces $ decl `sepEndBy` semiColon

decl :: P Decl
decl =  try (onPrio <$> stmt <* prio)
    <|> stmt
    where onPrio (Decl _ a b) = Decl (Just S.Important) a b
          stmt = Decl Nothing <$> (ident <* colon) <*> expr  

ident :: P Ident
ident = try (vendorIdent <$> vendor <*> plainIdent)
    <|> noVendorIdent <$> plainIdent
    where vendorIdent = S.Ident . Just
          noVendorIdent = S.Ident Nothing  

        
vendor :: P VendorPrefix
vendor = cssToken $ \x -> case x of
    T.VendorPrefix str  -> Just (parse str)
    _                   -> Nothing
    where parse x = case map toLower x of
            "-moz-"     -> Moz'
            "-webkit-"  -> Webkit'
            "-o-"       -> O'
            "-xv-"      -> Xv'
            "-ms-"      -> Ms'
            "mso-"      -> Mso'
            "-atsc-"    -> Atsc'
            "-wap-"     -> Wap'
            "-khtml-"   -> Khtml'
            "prince-"   -> Prince'
            "-ah-"      -> Ah'
            "-hp-"      -> Hp'
            "-ro-"      -> Ro'
            "-rim-"     -> Rim'
            "-tc-"      -> Tc'
            '_' : as    -> VendorUnder $ init as
            '-' : as    -> VendorMinus $ init as
            as          -> error $ msg as
          msg as = "error while parsing, wron vendor prefix: " ++ as


plainIdent :: P String
plainIdent = cssToken $ \x -> case x of
    T.Ident str -> Just str
    _           -> Nothing


expr :: P Expr 
expr = flip ($) <$> (EVal <$> value) <*> cont        
    where op f p = flip f <$> (p *> expr)
          cont =    op CommaSep comma  
                <|> op SlashSep slash
                <|> try (op SpaceSep empty) 
                <|> pure id                              


value :: P Value
value = VString     <$> string
    <|> VColor      <$> try color 
    <|> VUri        <$> try uri
    <|> VFunc       <$> try func
    <|> VPercentage <$> try percentage
    <|> try dimensionValue
    <|> VDouble     <$> try double    
    <|> VIdent      <$> ident


string :: P String
string = cssToken $ \x -> case x of
    StringTok str   -> Just str
    _               -> Nothing

color :: P Color
color = hash' <|> rgb' <|> hsl'
    where hash' = Cword . ('#':) <$> hash 
          rgb'  =  try (col  "rgb"  Crgb)  <|> try (colPt  "rgb"  CrgbPt)
               <|> try (cola "rgba" Crgba) <|> try (colaPt "rgba" CrgbaPt)
                
          hsl'  =  try (col  "hsl"  Chsl)  <|> try (colPt  "hsl"  ChslPt)
               <|> try (cola "hsla" Chsla) <|> try (colaPt "hsla" ChslaPt) 

          col    pref con = funcIs pref *> arg3 con int
          colPt  pref con = funcIs pref *> arg3 con percentage
          cola   pref con = funcIs pref *> arg4 con int
          colaPt pref con = funcIs pref *> arg4 con percentage
          
          arg3 f p = f <$> (p <* comma) <*> (p <* comma) <*> (p <* closeParen)
          arg4 f p = f <$> (p <* comma) <*> (p <* comma) <*> (p <* comma)
                        <*> (double <* closeParen)        

          funcIs a = cssToken $ \x -> case x of
            Function str    -> if (str == a) then Just () else Nothing
            _               -> Nothing

hash :: P String
hash = cssToken $ \x -> case x of
    Hash a  -> Just a
    _       -> Nothing

func :: P Func
func =  try (func3 <$> vendor <*> funcName <*> args)
    <|>     (func2 <$>            funcName <*> args) 
    where func3 vp name = Func (S.Ident (Just vp) name) 
          func2    name = Func (S.Ident Nothing   name)              
          args = (expr `sepBy1` comma) <* closeParen


funcName :: P String
funcName = cssToken $ \x -> case x of
    Function str    -> Just str
    _               -> Nothing

double :: P Double
double = cssToken $ \x -> case x of
   Number a     -> Just a
   _            -> Nothing

int :: P Int
int = round <$> double

uri :: P Uri
uri = cssToken $ \x -> case x of
    T.Uri a -> Just $ S.Uri a
    _       -> Nothing

percentage :: P Percentage
percentage = cssToken $ \x -> case x of
   T.Percentage a   -> Just $ S.Percentage a 
   _                -> Nothing

dimensionValue :: P Value
dimensionValue = cssToken $ \x -> case x of
    Dimension a m   -> Just $ parse m a
    _               -> Nothing
    where parse x = case map toLower x of
            "deg"   -> VDeg . Deg
            "rad"   -> VRad . Rad
            "grad"  -> VGrad . Grad
            "hz"    -> VHz . Hz
            "khz"   -> VKHz . KHz
            "em"    -> VEm . Em
            "ex"    -> VEx . Ex
            "px"    -> VPx . Px . round
            "in"    -> VIn . In 
            "cm"    -> VCm . Cm
            "mm"    -> VMm . Mm
            "pc"    -> VPc . Pc
            "pt"    -> VPt . Pt . round
            "ms"    -> VMs . Ms
            "s"     -> VS . S


prio :: P () 
prio = tok T.Important

atRule :: P (Maybe VendorPrefix, AtRule)
atRule = lol



---------------------------------------
--

tok :: Token -> P ()
tok t = cssToken $ fromBool . ( == t)
    where fromBool a 
            | a         = Just () 
            | otherwise = Nothing

cssToken :: (Token -> Maybe a) -> P a
cssToken test = token showT posT testT
    where showT (L _ t) = show t
          posT  (L p _) = pos2sourcePos p
          testT (L _ t) = test t

pos2sourcePos :: (Int, Int) -> SourcePos
pos2sourcePos (l,c) = newPos "" l c


parens, braces, brackets :: P a -> P a
parens   = between (tok OpenParen)  (tok CloseParen)
braces   = between (tok OpenCurly)  (tok CloseCurly)
brackets = between (tok OpenSquare) (tok CloseSquare)

closeParen :: P ()
closeParen = tok CloseParen

comma, colon, semiColon, period :: P ()
comma     = tok Comma
colon     = tok Colon
semiColon = tok SemiColon
period    = tok Period
slash     = tok Slash


