{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | Combinators to build AST
--
-- Example : 
--
-- >
-- >import Language.Css.Syntax
-- >import Language.Css.Build
-- >import Language.Css.Pretty
-- >import Language.Css.Build.Idents
-- >import Language.Css.Build.Tags hiding (center)
-- >
-- >res = ruleSets [
-- >         body [
-- >             margin =: int 0,
-- >             border =: int 0 ],
-- >            
-- >         h1 [ textAlign =: center],
-- >
-- >         p [ 
-- >            backgroundColor =: black, 
-- >            color =: white,
-- >            padding =: spaces [pct 5, pct 5, pct 10, pct 10]  ],
-- >
-- >         (star /. "warning") [ color =: red ] 
-- >       ]
-- > 
-- >main = print $ pretty res
-- >
--

module Language.Css.Build (
       -- * Classes
       Idents(..), ToExpr(..),

       -- * StyleSheet
       styleSheet, rules, ruleSets, 
       addImports, addNamespaces, addRules, charset,

       -- * AtRules
       media, page, fontFace, keyframes, at,

       -- * RuleSets

       -- ** Selectors
       Sel', star, sels, 
       (/>), (/-), (/#), (/.), (/:), (/::), (/~), 
       (!), (^=), ($=), (*=), (.=), (~=), (|=),

       -- ** Namespace prefix           
        SetNamespacePrefix(..), 
        anyNamespace, blankNamespace, justNamespace,

       -- ** Declarations
       (=:), (<:>), important, 
       space, slash, comma,
       spaces, slashes, commas,

       -- * Primitive values
       fun, str, int, num,
       deg, rad, grad, 
       cword, rgb, rgba, hsl, hsla, rgbPt, rgbaPt, hslPt, hslaPt,
       hz, khz, em, ex, px, in', cm, mm, pc, pt,
       pct, ms, s, url,

       -- * Colors
       aqua, black, blue, fuchsia, gray, green, 
       lime, maroon, navy, olive, orange, purple, 
       red, silver, teal, white, yellow
) where

import Language.Css.Syntax

import Control.Applicative

class Idents a where
    ident :: String -> a

instance Idents Ident where
    ident = Ident

instance Idents String where
    ident = id

class ToExpr a where
    expr :: a -> Expr

instance ToExpr Expr where
    expr = id

-----------------------------------------------------------------
-- StyleSheet

styleSheet :: Maybe AtCharSet -> [AtImport] -> [AtNamespace] 
    -> [StyleBody] -> StyleSheet
styleSheet = StyleSheet

-- | construct 'StyleSheet' from list of 'AtRule' 's or 'RuleSet' 's
rules :: [StyleBody] -> StyleSheet
rules = styleSheet Nothing [] []

-- | append imports
addImports :: [AtImport] -> StyleSheet -> StyleSheet
addImports is' (StyleSheet c is ns body) = StyleSheet c (is ++ is') ns body

-- | append namespaces
addNamespaces :: [AtNamespace] -> StyleSheet -> StyleSheet
addNamespaces ns' (StyleSheet c is ns body) = StyleSheet c is (ns ++ ns') body

-- | append rules
addRules :: [StyleBody] -> StyleSheet -> StyleSheet
addRules rs (StyleSheet c is ns body) = StyleSheet c is ns $ rs ++ body

-- | construct 'StyleSheet' from list of 'RuleSet' 's
ruleSets :: [RuleSet] -> StyleSheet
ruleSets = StyleSheet Nothing [] [] . map SRuleSet

-- | set \@charset
charset :: String -> StyleSheet -> StyleSheet
charset str (StyleSheet _ is ns body) = 
    StyleSheet (Just $ AtCharSet str) is ns body

-----------------------------------------------------------------
-- AtRules

-- | \@media
media :: [String] -> [RuleSet] -> StyleBody
media ms rs = SAtMedia $ AtMedia (map ident ms) rs

-- | \@page
page ::  Maybe String -> Maybe PseudoPage -> [Decl] -> StyleBody
page i p ds = SAtPage $ AtPage (ident <$> i) p ds

-- | import from string
importStr :: String -> [Ident] -> AtImport
importStr str = AtImport (IStr str)

-- | import from uri
importUri :: String -> [Ident] -> AtImport
importUri str = AtImport (IUri $ Uri str)

-- | \@font-face
fontFace :: [Decl] -> StyleBody
fontFace = SAtFontFace . AtFontFace

-- | \@keyframes
keyframes :: Ident -> [Frame] -> StyleBody 
keyframes name frames = SAtKeyframes $ AtKeyframes name frames

at :: Pt -> [Decl] -> Frame
at = Frame . FrameAt


-- RuleSets

-----------------------------------------------------------------
-- Selectors

infixl 5 /-, />, /+, /~
infixl 6 /#, /., !, /:, /::

infixr 0 <:>

-- | 'RuleSet' constructor
type Sel' = [Decl] -> RuleSet


-- | @*@ selector
star :: Sel'
star = RuleSet $ return $ SSel [UniversalSel Nothing]

instance Idents Sel' where
    ident x = RuleSet $ return $ SSel [TypeSel Nothing x]

-- compose

-- | groups selectors
sels :: [Sel'] -> Sel'
sels xs d = joinRules [] $ map ($ d) xs
    where joinRules sels xs = 
                case xs of
                    []   -> RuleSet sels d
                    a:as -> joinRules (sels ++ getSels a) as

-- | Descendant
--
-- space in css
(/-) :: Sel' -> Sel' -> Sel'
(/-) = liftSel2 $ CSel Descend

-- | Child
--
-- @>@ in css
(/>) :: Sel' -> Sel' -> Sel'
(/>) = liftSel2 $ CSel Child

-- | Adjacent sibling
--
-- @+@ in css
(/+) :: Sel' -> Sel' -> Sel'
(/+) = liftSel2 $ CSel Adjacent


-- | General sibling
--
-- @~@ in css
(/~) :: Sel' -> Sel' -> Sel'
(/~) = liftSel2 $ CSel Sibling


-- set attribs

-- | set id
(/#) :: Sel' -> String -> Sel'
(/#) s id = liftSel1 (appendSubSel $ IdSel id) s 

-- | set class
(/.) :: Sel' -> String -> Sel'
(/.) s cl = liftSel1 (appendSubSel $ ClassSel cl) s

-- | set attributes
(!) :: Sel' -> Attr -> Sel'
(!) s attr = liftSel1 (appendSubSel $ AttributeSel attr) s 

-- | set pseudo classes/elements (one colon)
(/:) :: Sel' -> PseudoVal -> Sel' 
(/:) = setPseudo OneColon

-- | set pseudo elements (two colons)
(/::) :: Sel' -> PseudoVal -> Sel'
(/::) = setPseudo TwoColons

setPseudo :: PseudoType -> Sel' -> PseudoVal -> Sel'
setPseudo pType s p = liftSel1 (appendSubSel $ PseudoSel pType p) s

liftSel1 :: (Sel -> Sel) -> (Sel' -> Sel')
liftSel1 f = liftA f'
    where f' a = RuleSet (liftA f $ getSels a) $ getDecls a

liftSel2 :: (Sel -> Sel -> Sel) -> (Sel' -> Sel' -> Sel')
liftSel2 f = liftA2 f' 
    where f' a b = RuleSet (liftA2 f (getSels a) (getSels b)) $ getDecls a


instance Idents Attr where
    ident x = Attr Nothing (ident x) Nothing 

instance Idents AttrVal where
    ident = AttrValIdent . ident


-- | element's attribute is prefix of
(^=) :: Attr -> AttrVal -> Attr
(^=) = setAttr PrefixMatch

-- | element's attribute is suffix of
($=) :: Attr -> AttrVal -> Attr
($=) = setAttr SuffixMatch

-- | element's attribute is substring of
(*=) :: Attr -> AttrVal -> Attr
(*=) = setAttr SubstringMatch

-- | element's attribute equals
(.=) :: Attr -> AttrVal -> Attr
(.=) = setAttr EqualsMatch

-- | element's attribute includes
(~=) :: Attr -> AttrVal -> Attr
(~=) = setAttr Includes

-- | element's attribute begins with
(|=) :: Attr -> AttrVal -> Attr
(|=) = setAttr DashMatch


appendSubSel :: SimpleSel -> Sel -> Sel
appendSubSel s a = case a of
    SSel xs     -> SSel $ xs ++ [s]     
    CSel op a b -> CSel op (appendSubSel s a) (appendSubSel s b)
        

getSels :: RuleSet -> [Sel] 
getSels (RuleSet xs _) = xs

getDecls :: RuleSet -> [Decl]
getDecls (RuleSet _ xs) = xs

setAttr :: AttrComb -> Attr -> AttrVal -> Attr
setAttr op a b = a{ attrRhs = Just $ AttrRhs op b } 

instance Idents PseudoVal where
    ident = PIdent . ident

-- namespace prefixes

class SetNamespacePrefix a where
    setNamespacePrefix :: NamespacePrefix -> a -> a

anyNamespace :: SetNamespacePrefix a => a -> a
anyNamespace = setNamespacePrefix AnyNamespace

blankNamespace :: SetNamespacePrefix a => a -> a
blankNamespace = setNamespacePrefix BlankNamespace

justNamespace :: SetNamespacePrefix a => String -> a -> a
justNamespace = setNamespacePrefix . JustNamespace . ident 


instance SetNamespacePrefix Sel' where
    setNamespacePrefix prefix = liftSel1 (setNamespacePrefix prefix)

instance SetNamespacePrefix SimpleSel where
    setNamespacePrefix prefix x = case x of
        UniversalSel _      -> UniversalSel (Just prefix)
        TypeSel _ elem      -> TypeSel (Just prefix) elem
        _                   -> x

instance SetNamespacePrefix Sel where
    setNamespacePrefix prefix x = case x of
        SSel as     -> SSel $ case as of
                        []      -> []
                        a:rest  -> phi a : rest
        CSel op a b -> CSel op (phi a) (phi b) 
        where phi :: SetNamespacePrefix a => a -> a
              phi = setNamespacePrefix prefix  


instance SetNamespacePrefix Attr where
    setNamespacePrefix prefix a = a{ attrNamespace = Just prefix }








-----------------------------------------------------------------
-- Declarations

-- | declaration constructor
(=:) :: String -> Expr -> Decl
(=:) a b = Decl Nothing (ident a) b


{-# DEPRECATED (<:>) "Use (=:) instead" #-}
-- | declaration constructor
(<:>) :: String -> Expr -> Decl
(<:>) a b = Decl Nothing (ident a) b

-- | set @!important@ 
important :: Decl -> Decl 
important (Decl _ a b) = Decl (Just Important) a b

-- | space separated values
space :: Expr -> Expr -> Expr
space = SpaceSep

-- | 'space' on list of values
spaces :: [Expr] -> Expr
spaces = foldl1 space

-- | slash separated values
slash :: Expr -> Expr -> Expr
slash = SlashSep

-- | 'slash' on list of values
slashes :: [Expr] -> Expr
slashes = foldl1 slash

-- | comma separated values
comma :: Expr -> Expr -> Expr
comma = CommaSep

-- | 'comma' on lists of values
commas :: [Expr] -> Expr
commas = foldl1 comma

instance ToExpr a => ToExpr [a] where
    expr x = case x of
                [] -> ident ""
                _  -> foldl1 space $ map expr x

-----------------------------------------------------------------
-- Values
--

instance Idents Expr where
    ident = EVal . ident

instance Idents Value where
    ident = VIdent . ident

instance ToExpr Value where 
    expr = EVal

-----------------------------------------------------------------
-- primitive values
-- constructors

-- | 'Func' constructor
fun :: ToExpr a => Ident -> a -> Func
fun str = Func str . return . expr

-- | \<angle\> 
deg :: Double -> Expr
deg = expr . Deg

-- | \<angle\> 
rad :: Double -> Expr
rad = expr . Rad

-- | \<angle\> 
grad :: Double -> Expr
grad = expr . Grad

-- | \<color\> 
cword :: String -> Expr
cword = expr . Cword . checkWord

-- | \<color\> 
rgb :: Int -> Int -> Int -> Expr
rgb x0 x1 x2 = expr $ Crgb x0 x1 x2

-- | \<color\> 
rgbPt :: Pt -> Pt -> Pt -> Expr
rgbPt x0 x1 x2 = expr $ CrgbPt x0 x1 x2

-- | \<color\> 
rgba :: Int -> Int -> Int -> Double -> Expr
rgba x0 x1 x2 a = expr $ Crgba x0 x1 x2 a

-- | \<color\> 
rgbaPt :: Pt -> Pt -> Pt -> Double -> Expr
rgbaPt x0 x1 x2 a = expr $ CrgbaPt x0 x1 x2 a

-- | \<color\>
hsl :: Int -> Int -> Int -> Expr
hsl x0 x1 x2 = expr $ Chsl x0 x1 x2

-- | \<color\> 
hslPt :: Pt -> Pt -> Pt -> Expr
hslPt x0 x1 x2 = expr $ ChslPt x0 x1 x2

-- | \<color\>
hsla :: Int -> Int -> Int -> Double -> Expr
hsla x0 x1 x2 a = expr $ Chsla x0 x1 x2 a

-- | \<color\> 
hslaPt :: Pt -> Pt -> Pt -> Double -> Expr
hslaPt x0 x1 x2 a = expr $ ChslaPt x0 x1 x2 a


-- | \<frequency\> 
hz :: Double -> Expr
hz = expr . Hz

-- | \<frequency\> 
khz :: Double -> Expr
khz = expr . KHz

-- | \<length\> 
em :: Double -> Expr
em = expr . Em

-- | \<length\> 
ex :: Double -> Expr
ex = expr . Ex

-- | \<length\> 
px :: Int -> Expr
px = expr . Px

-- | \<length\> 
in' :: Double -> Expr
in' = expr . In

-- | \<length\> 
cm :: Double -> Expr
cm = expr . Cm

-- | \<length\> 
mm :: Double -> Expr
mm = expr . Mm

-- | \<length\> 
pc :: Double -> Expr
pc = expr . Pc

-- | \<length\> 
pt :: Int -> Expr
pt = expr . Pt

-- | \<percentage\> 
pct :: Double -> Expr
pct = expr . Percentage

-- | \<time\> 
ms :: Double -> Expr
ms = expr . Ms

-- | \<time\> 
s :: Double -> Expr
s = expr . S

-- | \<uri\> 
url :: String -> Expr
url = expr . Uri

checkWord x 
    | checkLeng x && checkNums x && checkFirst x = x
    | otherwise                                  = errorMsg 
    where errorMsg = error "must be number in hexadecimal notation" 
          checkLeng x 
            | length x == 4 || length x == 7 = True
            | otherwise = error "string length must be 4 or 7"
          checkNums x
            | all (`elem` (['0' .. '9'] ++ ['a' .. 'f'] ++ ['A' .. 'F']))$ tail x = True
            | otherwise = errorMsg
          checkFirst x
            | '#' == head x = True
            | otherwise = error "first character must be #"        



str :: String -> Expr
str = expr . VString

int :: Int -> Expr
int = expr

num :: Double -> Expr 
num = expr

-- instances

-- ToExpr

instance ToExpr Deg where
    expr = expr . VDeg

instance ToExpr Rad where
    expr = expr . VRad

instance ToExpr Grad where
    expr = expr . VGrad

instance ToExpr Color where
    expr = expr . VColor

instance ToExpr Hz where
    expr = expr . VHz

instance ToExpr KHz where
    expr = expr . VKHz

instance ToExpr Func where
    expr = expr . VFunc

instance ToExpr Ident where
    expr = expr . VIdent

instance ToExpr Int where
    expr = expr . VInt

instance ToExpr Em where
    expr = expr . VEm

instance ToExpr Ex where
    expr = expr . VEx

instance ToExpr Px where
    expr = expr . VPx

instance ToExpr In where
    expr = expr . VIn

instance ToExpr Cm where
    expr = expr . VCm

instance ToExpr Mm where
    expr = expr . VMm

instance ToExpr Pc where
    expr = expr . VPc

instance ToExpr Pt where
    expr = expr . VPt

instance ToExpr Double where
    expr = expr . VDouble

instance ToExpr Percentage where
    expr = expr . VPercentage

instance ToExpr Ms where
    expr = expr . VMs

instance ToExpr S where
    expr = expr . VS

instance ToExpr Uri where
    expr = expr . VUri

instance ToExpr Nth where
    expr = expr . VNth

-- colors
--
aqua    = cword "#00ffff" 
black   = cword "#000000"
blue    = cword "#0000ff" 
fuchsia = cword "#ff00ff" 
gray    = cword "#808080"  
green   = cword "#008000" 
lime    = cword "#00ff00"  
maroon  = cword "#800000"  
navy    = cword "#000080" 
olive   = cword "#808000" 
orange  = cword "#ffA500" 
purple  = cword "#800080" 
red     = cword "#ff0000"  
silver  = cword "#c0c0c0" 
teal    = cword "#008080" 
white   = cword "#ffffff" 
yellow  = cword "#ffff00"



