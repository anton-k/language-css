-- todo AtCharSet -> AtCharset

-- parsec tuts
-- http://www.haskell.org/haskellwiki/Parsing_a_simple_imperative_language
-- http://www.haskell.org/haskellwiki/Parsing_expressions_and_statements
module Parser where

import Control.Applicative
import Control.Monad

import Data.Char(toLower)

import Text.ParserCombinators.Parsec 
    hiding (many, (<|>), string)

import Text.ParserCombinators.Parsec.Pos

import Language.Css.Syntax
import Language.Css.Pretty(Pretty(..))
import qualified Language.Css.Syntax as S

import Token
import qualified Token as T

type P = GenParser (L Token) ()

lol = undefined

parser :: P a -> [L Token] -> Either ParseError a
parser p = runParser p () ""

-----------------------------------------------------
-- top level

styleSheet :: P StyleSheet
styleSheet = sl $ StyleSheet <$> many rule

rule :: P Rule
rule =  sr $ 
        atRule 
    <|> SRuleSet <$> ruleSet 

------------------------------------------------------
-- at rules

atRule :: P Rule
atRule = join $ fmap sr $ cssToken $ \x -> case x of
    T.AtRule vp name -> Just $ SAtRule (parseVendor <$> vp) <$> parse name
    _                -> Nothing
    where parse x = case map toLower x of
            "charset"   -> charset
            "import"    -> import'
            "namespace" -> namespace
            "media"     -> media
            "page"      -> page
            "font-face" -> fontFace
            "keyframes" -> keyframes
            as          -> error $ msg as
          msg as = "parsing wrong at-rule: @" ++ as

charset, import', namespace, media, 
    page, fontFace, keyframes :: P AtRule

charset = sb $ AtCharset <$> string <* semiColon           

import' = sb $ AtImport <$> importHead <*> medias 
    where medias = (ident `sepBy` comma) <* semiColon 
    
importHead = sb $ IStr <$> string <|> IUri <$> uri

namespace = sb $ AtNamespace <$> optionMaybe ident <*> importHead <* semiColon

media = sb $ AtMedia <$> mediums <*> rules
    where mediums = (ident `sepBy1` comma)
          rules = braces $ many ruleSet   

page = sb $ AtPage <$> optionMaybe (colon *> ident) <*> decls 

fontFace = sb $ AtFontFace <$> decls

keyframes = sb $ AtKeyframes <$> ident <*> frames
    where frames = braces $ many frame
          frame  = sr $ Frame <$> time <*> decls
          time   = sr $ from <|> to <|> FrameAt <$> percent  
                where from = sr $ From <$ identIs "from"
                      to   = sr $ To   <$ identIs "to"  

------------------------------------------------------
-- rules

ruleSet :: P RuleSet
ruleSet = sr $ RuleSet <$> groupSel <*> decls

prio :: P Prio 
prio = sr $ S.Important <$ tok T.Important

groupSel :: P GroupSel
groupSel = sr $ sel `sepBy1` comma

sel :: P Sel
sel = flip ($) <$> ssel <*> cont
    where op f p = try $ flip (CSel f) <$> (p *> sel)   
          cont = op Descend     s1       -- space separates
             <|> op Adjacent    plus
             <|> op S.Child     greater
             <|> op Sibling     tilde
             <|> pure id
    
ssel :: P Sel
ssel = sl $ SSel <$> (Just <$> typeSel) <*> many simpleSel 
             <|> SSel Nothing <$> many1 simpleSel

typeSel :: P TypeSel
typeSel = try (conNamespace <$> namespacePrefix <*> elementSel)
      <|> conNoNamespace <$> elementSel
    where conNamespace   = TypeSel . Just
          conNoNamespace = TypeSel Nothing
    

namespacePrefix :: P NamespacePrefix
namespacePrefix = sr $ 
        BlankNamespace <$ bar
    <|> AnyNamespace <$ (star *> bar)
    <|> JustNamespace <$> (ident <* bar)

elementSel :: P ElementSel
elementSel = UniversalSel <$ star 
        <|>  ElementSel <$> ident

simpleSel :: P SimpleSel
simpleSel =  
        idSel
    <|> classSel
    <|> attributeSel
    <|> pseudoSel
    <|> negationSel 

idSel       = IdSel <$> hash 

classSel    = ClassSel . joinName <$> (period *> ident)
    where joinName (S.Ident a b) = maybe id ((++) . printPrefix) a b        
          printPrefix = show . pretty  

attributeSel = AttributeSel <$> brackets 
    (try attrNamespace <|> attrNoNamespace)
    where attrNamespace = Attr <$> (Just <$> namespacePrefix) <*> ident 
                      <*> optionMaybe attrRhs
          attrNoNamespace = Attr Nothing <$> ident <*> optionMaybe attrRhs
          attrRhs = sr $ AttrRhs <$> attrComb <*> attrVal
          attrComb = sr $
                     prefixMatch <|> suffixMatch <|> substringMatch 
                 <|> equalsMatch <|> includes <|> dashMatch
          attrVal = sr $
                    AttrValIdent  <$> ident
                <|> AttrValString <$> string

prefixMatch     = sr $ S.PrefixMatch     <$ tok T.PrefixMatch
suffixMatch     = sr $ S.SuffixMatch     <$ tok T.SuffixMatch
dashMatch       = sr $ S.DashMatch       <$ tok T.DashMatch 
substringMatch  = sr $ S.SubstringMatch	<$ tok T.SubstringMatch	
equalsMatch	    = sr $ S.EqualsMatch	    <$ tok T.EqualsMatch	
includes        = sr $ S.Includes        <$ tok T.Includes


pseudoSel = colon *> (PseudoSel <$> pseudoType <*> pseudoVal)

pseudoType = sr $ option OneColon (TwoColons <$ colon)
pseudoVal = PFunc  <$> func 
        <|> PIdent <$> ident

negationSel = 
    NegationSel <$> (funcIs "not" *> negationArg <* closeParen)

negationArg = sr $ 
        NegationArg1 <$> typeSel
    <|> NegationArg2 <$> simpleSel
          
decls :: P [Decl]
decls = sr $ braces $ decl `sepEndBy` semiColon

decl :: P Decl
decl = sr $ comb <$> (ident <* sl colon) <*> expr <*> optionMaybe prio
    where comb a b c = Decl c a b


ident :: P Ident
ident = S.Ident <$> optionMaybe vendor <*> plainIdent

identIs :: String -> P ()
identIs a = sr $ cssToken $ \x -> case x of
    T.Ident b   -> if a == b then Just () else Nothing
    _           -> Nothing

vendor :: P VendorPrefix
vendor = cssToken $ \x -> case x of
    T.VendorPrefix str  -> Just (parseVendor str)
    _                   -> Nothing
    
parseVendor :: String -> VendorPrefix
parseVendor x = case map toLower x of
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
    where msg as = "parsing wrong vendor prefix: " ++ as


plainIdent :: P String
plainIdent = cssToken $ \x -> case x of
    T.Ident str -> Just str
    _           -> Nothing


expr :: P Expr 
expr = sr $ flip ($) <$> (EVal <$> value) <*> cont        
    where op f p = sr $ flip f <$> (p *> expr)
          cont =    op CommaSep comma  
                <|> op SlashSep slash
                <|> try (flip SpaceSep <$> expr) 
                <|> pure id                              


------------------------------------------------
-- values

value :: P Value
value = sr $ 
        VString     <$> string
    <|> VColor      <$> try color 
    <|> VUri        <$> try uri
    <|> VFunc       <$> try func
    <|> VPercent <$> try percent
    <|> try dimensionValue
    <|> VDouble     <$> try double    
    <|> VIdent      <$> ident


string :: P String
string = sr $ cssToken $ \x -> case x of
    StringTok str   -> Just str
    _               -> Nothing

color :: P Color
color = sr $ hash' <|> rgb' <|> hsl'
    where hash' = Cword . ('#':) <$> hash 
          rgb'  =  try (col  "rgb"  Crgb)  <|> try (colPt  "rgb"  CrgbPt)
               <|> try (cola "rgba" Crgba) <|> try (colaPt "rgba" CrgbaPt)
                
          hsl'  =  try (col  "hsl"  Chsl)  <|> try (colPt  "hsl"  ChslPt)
               <|> try (cola "hsla" Chsla) <|> try (colaPt "hsla" ChslaPt) 

          col    pref con = sr $ funcIs pref *> arg3 con int
          colPt  pref con = sr $ funcIs pref *> arg3 con percent
          cola   pref con = sr $ funcIs pref *> arg4 con int
          colaPt pref con = sr $ funcIs pref *> arg4 con percent
          
          arg3 f p = sr $
                     f <$> (p <* comma) <*> (p <* comma) <*> (p <* closeParen)
          arg4 f p = sr $
                     f <$> (p <* comma) <*> (p <* comma) <*> (p <* comma)
                       <*> (double <* closeParen)        

funcIs :: String -> P ()
funcIs a = sr $ cssToken $ \x -> case x of
    Function str    -> if (map toLower str == a) then Just () else Nothing
    _               -> Nothing

hash :: P String
hash = cssToken $ \x -> case x of
    Hash a  -> Just a
    _       -> Nothing

func :: P Func
func = sr $
        try (func3 <$> vendor <*> funcName <*> args)
    <|> (func2 <$>            funcName <*> args) 
    where func3 vp name = Func (S.Ident (Just vp) name) 
          func2    name = Func (S.Ident Nothing   name)              
          args = (expr `sepBy1` comma) <* closeParen


funcName :: P String
funcName = sr $ cssToken $ \x -> case x of
    Function str    -> Just str
    _               -> Nothing

double :: P Double
double = sr $ cssToken $ \x -> case x of
   Number a     -> Just a
   _            -> Nothing

int :: P Int
int = sr $ round <$> double

uri :: P Uri
uri = sr $ cssToken $ \x -> case x of
    T.Uri a -> Just $ S.Uri a
    _       -> Nothing

percent :: P Percent
percent = sr $ cssToken $ \x -> case x of
   T.Percent a   -> Just $ S.Percent a 
   _                -> Nothing

dimensionValue :: P Value
dimensionValue = sr $ cssToken $ \x -> case x of
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
            as      -> error $ msg as
          msg as = "parsing wrong dimension specifier: " ++ as

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
parens   = between (sr $ tok OpenParen)  (tok CloseParen)
braces   = between (sr $ tok OpenCurly)  (tok CloseCurly)
brackets = between (sr $ tok OpenSquare) (tok CloseSquare)

closeParen :: P ()
closeParen = tok CloseParen

s, s1 :: P [()]

s  = many  space'
s1 = many1 space'

sl, sr, sb :: P a -> P a

sl = (s *>)
sr = ( <* s)

sb = sl . sr

space' :: P ()
space' = tok Space



comma, colon, semiColon, period :: P ()

comma     = sr $ tok Comma
colon     = sr $ tok Colon
semiColon = sr $ tok SemiColon
period    = sr $ tok Period
slash     = sr $ tok Slash
tilde     = sb $ tok Tilde
greater   = sb $ tok Greater  
plus      = sb $ tok Plus
star      = sr $ tok Mult  
bar       = sr $ tok Bar

