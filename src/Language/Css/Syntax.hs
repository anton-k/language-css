
-- | Css2.1 syntax and bits of Css 3
-- 
-- Css3:  selectors, namespaces, colors, keyframes
--
-- haskell translation of css 2.1 grammar. 
--
-- See <http://www.w3.org/TR/CSS2/grammar.html> and <http://www.w3.org/TR/CSS2/syndata.html>
module Language.Css.Syntax (
        -- * Stylesheet
        StyleSheet(..), Rule(..),

        -- * AtRule
        AtRule(..),
        ImportHead(..), PseudoPage, Frame(..), FrameTime(..),
        
        -- * RuleSet
        RuleSet(..), Decl(..), Prop, Prio(..), Expr(..),
        
        -- * Selectors
        GroupSel, Sel(..), SelComb(..), SimpleSel(..), 
        TypeSel(..), ElementSel(..),
        NamespacePrefix(..), PseudoType(..),  
        Attr(..), AttrRhs(..), AttrComb(..), AttrVal(..),
        Element, Class, Id, PseudoVal(..), NegationArg(..),

        -- * Values
        Value(..), 
       
        -- * Primitives
        Ident(..), VendorPrefix(..), Func(..), 
                
        Deg(..), 
        Rad(..),
        Grad(..),
        Color(..),
        Hz(..),
        KHz(..),
        Em(..),
        Ex(..),
        Px(..),
        In(..),
        Cm(..),
        Mm(..),
        Pc(..),
        Pt(..),
        Percent(..),
        Ms(..),
        S(..),
        Uri(..),
        Nth(..)
    ) where


import Text.PrettyPrint

data Ident = Ident (Maybe VendorPrefix) String
                  deriving (Eq, Show)


data VendorPrefix = 
      Ms' | Mso' | Moz' | O' | Xv' | Atsc' | Wap' | Khtml' 
    | Webkit' | Prince' | Ah' | Hp' | Ro' | Rim' | Tc' 
    | VendorUnder String | VendorMinus String
    deriving (Eq, Show)


--------------------------------------------------------
-- Stylesheet

newtype StyleSheet = StyleSheet [Rule]
    deriving (Eq, Show)

data Rule = SRuleSet     RuleSet 
          | SAtRule (Maybe VendorPrefix) AtRule
                deriving (Eq, Show)

---------------------------------------------------------
-- AtRules

data AtRule 
        = AtCharset String                      -- ^ \@charset
        | AtImport ImportHead [Ident]           -- ^ \@import
        | AtNamespace (Maybe Ident) ImportHead  -- ^ \@namespace
        | AtMedia [Ident] [RuleSet]             -- ^ \@media
        | AtPage (Maybe PseudoPage) [Decl]      -- ^ \@page
        | AtFontFace [Decl]                     -- ^ \@font-face
        | AtKeyframes Ident [Frame]             -- ^ \@keyframes 
        deriving (Eq, Show)  

data ImportHead =  IStr String | IUri Uri
                  deriving (Eq, Show)  

type PseudoPage = Ident

data Frame = Frame FrameTime [Decl]
                  deriving (Eq, Show)  

   
data FrameTime = From | To | FrameAt Percent
                  deriving (Eq, Show)  


---------------------------------------------------------
-- Rules

data RuleSet = RuleSet GroupSel [Decl]
                  deriving (Eq, Show)

-- | Declaration
data Decl = Decl (Maybe Prio) Prop Expr
                  deriving (Eq, Show)
-- | Property
type Prop = Ident

-- | sets @!important@ declaration
data Prio = Important
    deriving (Eq, Show)

---------------------------------------------------------
-- Selectors

type GroupSel = [Sel]

-- | Selector
data Sel = SSel (Maybe TypeSel) [SimpleSel] -- ^ simple selector	 
         | CSel SelComb Sel Sel             -- ^ combination of selectors
    deriving (Eq, Show)

data SelComb 
    = Descend       -- ^ ' '
    | Child         -- ^ \'>\'
    | Adjacent      -- ^ \'+\'
    | Sibling       -- ^ '~'
    deriving (Eq, Show)


data TypeSel = TypeSel (Maybe NamespacePrefix) ElementSel
    deriving (Eq, Show)

data ElementSel = UniversalSel | ElementSel Ident
    deriving (Eq, Show)

data SimpleSel 
    = AttributeSel Attr                           -- ^ attribute selector
    | ClassSel Class                              -- ^ class selector
    | IdSel Id                                    -- ^ id selector
    | PseudoSel PseudoType PseudoVal              -- ^ pseudo /class/element
    | NegationSel NegationArg                     -- ^ negation selector
    deriving (Eq, Show)

data PseudoType = OneColon | TwoColons
    deriving (Eq, Show)

data NamespacePrefix 
    = AnyNamespace          -- ^ '*|Element'
    | BlankNamespace        -- ^ ' |Element'
    | JustNamespace Ident   -- ^ 'foo|Element'
    deriving (Eq, Show)

-- | attribute selector
data Attr = Attr 
    { attrNamespace :: Maybe NamespacePrefix
    , attrIdent     :: Ident 
    , attrRhs       :: Maybe AttrRhs
    } deriving (Eq, Show)

data AttrRhs = AttrRhs AttrComb AttrVal
    deriving (Eq, Show)

data AttrComb   = PrefixMatch       -- ^ \'^=\'      
                | SuffixMatch       -- ^ \'$=\'
                | SubstringMatch    -- ^ \'*=\'
                | EqualsMatch       -- ^ \'=\'
                | Includes          -- ^ \'~=\'          
                | DashMatch         -- ^ \'|=\'
                  deriving (Eq, Show)

data AttrVal    = AttrValIdent Ident 
                | AttrValString String
                  deriving (Eq, Show)

type Element = String
type Class = String
type Id = String
	
data PseudoVal = PIdent Ident
               | PFunc Func
                  deriving (Eq, Show)

data NegationArg 
    = NegationArg1 TypeSel
    | NegationArg2 SimpleSel
    deriving (Eq, Show)



-------------------------------------------------------------------
-- Values

data Expr = EVal Value            -- ^ single value
          | SlashSep Expr Expr    -- ^ slash separated expressions
          | CommaSep Expr Expr    -- ^ comma separated expressions
          | SpaceSep Expr Expr    -- ^ space separated expressions
                  deriving (Eq, Show)

data Value  = VDeg Deg 
            | VRad Rad 
            | VGrad Grad 
            | VColor Color 
            | VHz Hz 
            | VKHz KHz 
            | VFunc Func 
            | VIdent Ident 
            | VInt Int 
            | VEm Em 
            | VEx Ex 
            | VPx Px 
            | VIn In 
            | VCm Cm 
            | VMm Mm 
            | VPc Pc 
            | VPt Pt 
            | VDouble Double 
            | VPercent Percent 
            | VString String 
            | VMs Ms 
            | VS S 
            | VUri Uri 
            | VNth Nth
        deriving (Eq, Show)

-- | Function is identifier and the list of arguments
data Func = Func Ident [Expr]
    deriving (Eq, Show)

-- | \<angle\>
data Deg = Deg Double
        deriving (Eq, Show)

-- | \<angle\>
data Rad = Rad Double
        deriving (Eq, Show)

-- | \<angle\>
data Grad = Grad Double
        deriving (Eq, Show)

-- | \<color\>
data Color = Cword String 

           | Crgb Int Int Int
           | CrgbPt Percent Percent Percent           
           | Crgba Int Int Int Double
           | CrgbaPt Percent Percent Percent Double
           
           | Chsl Int Int Int
           | ChslPt Percent Percent Percent
           | Chsla Int Int Int Double
           | ChslaPt Percent Percent Percent Double
        deriving (Eq, Show)

-- | \<frequency\>
data Hz = Hz Double
        deriving (Eq, Show)

-- | \<frequency\>
data KHz = KHz Double
        deriving (Eq, Show)

-- | \<length\>
data Em = Em Double
        deriving (Eq, Show)

-- | \<length\>
data Ex = Ex Double
        deriving (Eq, Show)

-- | \<length\>
data Px = Px Int
        deriving (Eq, Show)

-- | \<length\>
data In = In Double
        deriving (Eq, Show)

-- | \<length\>
data Cm = Cm Double
        deriving (Eq, Show)

-- | \<length\>
data Mm = Mm Double
        deriving (Eq, Show)

-- | \<length\>
data Pc = Pc Double
        deriving (Eq, Show)

-- | \<length\>
data Pt = Pt Int
        deriving (Eq, Show)

-- | \<percentage\>
data Percent = Percent Double
        deriving (Eq, Show)

-- | \<time\>
data Ms = Ms Double
        deriving (Eq, Show)

-- | \<time\>
data S = S Double
        deriving (Eq, Show)

-- | \<uri\>
data Uri = Uri String
        deriving (Eq, Show)


-- | 'a*n+b' values
data Nth    = N Int Int     -- ^ 'a*n+b'
            | Nth Int       -- ^ 'n'
            | Odd
            | Even
        deriving (Eq, Show)
