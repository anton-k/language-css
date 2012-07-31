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
styleSheet = StyleSheet <$> many rule

rule :: P Rule
rule =  atRule 
    <|> SRuleSet <$> ruleSet 

------------------------------------------------------
-- at rules

atRule :: P Rule
atRule = join $ cssToken $ \x -> case x of
    T.AtRule vp name    -> Just $ SAtRule (parseVendor <$> vp) <$> parse name
    _                   -> Nothing
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

charset = AtCharset <$> string <* semiColon           

import' = AtImport <$> importHead <*> medias 
    where medias = (ident `sepBy` comma) <* semiColon 
    
importHead =  IStr <$> string <|> IUri <$> uri

namespace = AtNamespace <$> optionMaybe ident <*> importHead <* semiColon

media = AtMedia <$> mediums <*> rules
    where mediums = (ident `sepBy1` comma)
          rules = braces $ many ruleSet   

page = AtPage <$> optionMaybe (colon *> ident) <*> decls 

fontFace = AtFontFace <$> decls

keyframes = AtKeyframes <$> ident <*> frames
    where frames = braces $ many frame
          frame  = Frame <$> time <*> decls
          time   = from <|> to <|> FrameAt <$> percent  
                where from = From <$ identIs "from"
                      to   = To   <$ identIs "to"  

------------------------------------------------------
-- rules

ruleSet :: P RuleSet
ruleSet = RuleSet <$> groupSel <*> decls

prio :: P () 
prio = tok T.Important

groupSel :: P GroupSel
groupSel = sel `sepBy1` comma

sel :: P Sel
sel = flip ($) <$> ssel <*> cont
    where ssel = SSel <$> (Just <$> typeSel) <*> many simpleSel 
             <|> SSel Nothing <$> many1 simpleSel
          op f p = flip (CSel f) <$> (p *> sel)   
          cont = op Adjacent    plus
             <|> op S.Child     greater
             <|> op Sibling     tilde
             <|> try (flip (CSel Descend) <$> sel)     -- space separates
             <|> pure id

typeSel :: P TypeSel
typeSel = try (conNamespace <$> namespacePrefix <*> elementSel)
      <|> conNoNamespace <$> elementSel
    where conNamespace   = TypeSel . Just
          conNoNamespace = TypeSel Nothing
    

namespacePrefix :: P NamespacePrefix
namespacePrefix = BlankNamespace <$ bar
    <|> AnyNamespace <$ (star *> bar)
    <|> JustNamespace <$> (ident <* bar)

elementSel :: P ElementSel
elementSel = UniversalSel <$ star 
        <|>  ElementSel <$> ident

simpleSel :: P SimpleSel
simpleSel = idSel
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
          attrRhs = AttrRhs <$> attrComb <*> attrVal
          attrComb = prefixMatch <|> suffixMatch <|> substringMatch 
                 <|> equalsMatch <|> includes <|> dashMatch
          attrVal = AttrValIdent  <$> ident
                <|> AttrValString <$> string

prefixMatch     = S.PrefixMatch     <$ tok T.PrefixMatch
suffixMatch     = S.SuffixMatch     <$ tok T.SuffixMatch
dashMatch       = S.DashMatch       <$ tok T.DashMatch 
substringMatch  = S.SubstringMatch	<$ tok T.SubstringMatch	
equalsMatch	    = S.EqualsMatch	    <$ tok T.EqualsMatch	
includes        = S.Includes        <$ tok T.Includes


pseudoSel = colon *> (PseudoSel <$> pseudoType <*> pseudoVal)

pseudoType = option OneColon (TwoColons <$ colon)
pseudoVal = PFunc  <$> func 
        <|> PIdent <$> ident

negationSel = NegationSel <$> (funcIs "not" *> negationArg <* closeParen)

negationArg = NegationArg1 <$> typeSel
          <|> NegationArg2 <$> simpleSel
          
decls :: P [Decl]
decls = braces $ decl `sepEndBy` semiColon

decl :: P Decl
decl =  try (onPrio <$> stmt <* prio)
    <|> stmt
    where onPrio (Decl _ a b) = Decl (Just S.Important) a b
          stmt = Decl Nothing <$> (ident <* colon) <*> expr  

ident :: P Ident
ident = S.Ident <$> optionMaybe vendor <*> plainIdent

identIs :: String -> P ()
identIs a = cssToken $ \x -> case x of
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
expr = flip ($) <$> (EVal <$> value) <*> cont        
    where op f p = flip f <$> (p *> expr)
          cont =    op CommaSep comma  
                <|> op SlashSep slash
                <|> try (flip SpaceSep <$> expr) 
                <|> pure id                              


------------------------------------------------
-- values

value :: P Value
value = VString     <$> string
    <|> VColor      <$> try color 
    <|> VUri        <$> try uri
    <|> VFunc       <$> try func
    <|> VPercent <$> try percent
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
          colPt  pref con = funcIs pref *> arg3 con percent
          cola   pref con = funcIs pref *> arg4 con int
          colaPt pref con = funcIs pref *> arg4 con percent
          
          arg3 f p = f <$> (p <* comma) <*> (p <* comma) <*> (p <* closeParen)
          arg4 f p = f <$> (p <* comma) <*> (p <* comma) <*> (p <* comma)
                        <*> (double <* closeParen)        

funcIs :: String -> P ()
funcIs a = cssToken $ \x -> case x of
    Function str    -> if (map toLower str == a) then Just () else Nothing
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

percent :: P Percent
percent = cssToken $ \x -> case x of
   T.Percent a   -> Just $ S.Percent a 
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
tilde     = tok Tilde
greater   = tok Greater  
plus      = tok Plus
star      = tok Mult  
bar       = tok Bar
