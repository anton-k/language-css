
-- | Css2.1 syntax
--
-- haskell translation of css 2.1 grammar. 
--
-- See <http://www.w3.org/TR/CSS2/grammar.html> and <http://www.w3.org/TR/CSS2/syndata.html>
module Language.Css.Syntax (
        -- * Stylesheet
        StyleSheet(..), StyleBody(..),

        -- * AtRule
        AtCharSet(..), 
        AtImport(..), ImportHead(..),
        AtNamespace(..), NamespacePrefix,
        AtMedia(..), 
        AtPage(..), PseudoPage,
        AtFontFace(..),
        AtKeyframes(..), Frame(..), FrameTime(..),
        
        -- * RuleSet
        RuleSet(..), Decl(..), Prop, Prio(..), Expr(..),
        
        -- * Selectors
        Sel(..), SimpleSel(..), SubSel(..),
        Element, Attr(..), Class, Id, AttrIdent, AttrVal, PseudoVal(..),

        -- * Values
        Value(..), 
       
        -- * Primitives
        Ident(..), Func(..), 
                
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
        Percentage(..),
        Ms(..),
        S(..),
        Uri(..)

    ) where


import Text.PrettyPrint

data Ident = Ident String
                  deriving (Eq, Show)

--------------------------------------------------------
-- Stylesheet

data StyleSheet = 
    StyleSheet (Maybe AtCharSet) [AtImport] [AtNamespace] [StyleBody]
    deriving (Eq, Show)

data StyleBody = SRuleSet     RuleSet 
               | SAtMedia     AtMedia 
               | SAtPage      AtPage
               | SAtFontFace  AtFontFace
               | SAtKeyframes AtKeyframes
                  deriving (Eq, Show)

---------------------------------------------------------
-- AtRules

-- | \@charset
data AtCharSet = AtCharSet String
                  deriving (Eq, Show)

-- | \@import
data AtImport = AtImport ImportHead [Ident]
                  deriving (Eq, Show)

data ImportHead = IStr String | IUri Uri
                  deriving (Eq, Show)

-- | \@namespace
data AtNamespace = AtNamespace (Maybe NamespacePrefix) ImportHead 
                  deriving (Eq, Show)

type NamespacePrefix = Ident


-- | \@media
data AtMedia = AtMedia [Ident] [RuleSet]
                  deriving (Eq, Show)

-- | \@page
data AtPage = AtPage (Maybe Ident) (Maybe PseudoPage) [Decl]
                  deriving (Eq, Show)

type PseudoPage = Ident

-- | \@font-face
data AtFontFace = AtFontFace [Decl]
                  deriving (Eq, Show)

-- | \@keyframes 
data AtKeyframes = AtKeyframes Ident [Frame]
                  deriving (Eq, Show)  

data Frame = Frame FrameTime [Decl]
                  deriving (Eq, Show)  

   
data FrameTime = From | To | FrameAt Pt
                  deriving (Eq, Show)  


---------------------------------------------------------
-- Rules

data RuleSet = RuleSet [Sel] [Decl]
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

-- | Selector
data Sel = SSel SimpleSel    -- ^ single selector	 
	 | DescendSel Sel Sel    -- ^ ' '
	 | ChildSel   Sel Sel    -- ^ \'>\'
	 | AdjSel     Sel Sel    -- ^ \'+\'
                  deriving (Eq, Show)

-- | Simple selector
data SimpleSel = UnivSel [SubSel]           -- ^ Universal selector
    	       | TypeSel Element [SubSel]   -- ^ Type selector
                  deriving (Eq, Show)


data SubSel = AttrSel Attr        -- ^ attribute selector
	    | ClassSel Class          -- ^ \'.\'
	    | IdSel Id                -- ^ \'#\'
	    | PseudoSel PseudoVal     -- ^ pseudo classes/elements
                  deriving (Eq, Show)

-- | attribute selector
data Attr = Attr AttrIdent                
          | AttrIs AttrIdent AttrVal      -- ^ \'=\'
          | AttrIncl AttrIdent AttrVal    -- ^ \'~=\'
          | AttrBegins AttrIdent AttrVal  -- ^ \'|=\'
                  deriving (Eq, Show)

type Element = String
type Class = String
type Id = String
	
type AttrIdent = String
type AttrVal = String

data PseudoVal = PIdent Ident
               | PFunc Func
                  deriving (Eq, Show)

-------------------------------------------------------------------
-- Values

data Expr = EVal Value            -- ^ single value
          | SlashSep Expr Expr    -- ^ slash separated expressions
          | CommaSep Expr Expr    -- ^ comma separated expressions
          | SpaceSep Expr Expr    -- ^ space separated expressions
                  deriving (Eq, Show)

data Value = VDeg Deg | 
             VRad Rad | 
             VGrad Grad | 
             VColor Color | 
             VHz Hz | 
             VKHz KHz | 
             VFunc Func | 
             VIdent Ident | 
             VInt Int | 
             VEm Em | 
             VEx Ex | 
             VPx Px | 
             VIn In | 
             VCm Cm | 
             VMm Mm | 
             VPc Pc | 
             VPt Pt | 
             VDouble Double | 
             VPercentage Percentage | 
             VString String | 
             VMs Ms | 
             VS S | 
             VUri Uri
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
           | CrgbPt Pt Pt Pt           
           | Crgba Int Int Int Double
           | CrgbaPt Pt Pt Pt Double
           
           | Chsl Int Int Int
           | ChslPt Pt Pt Pt
           | Chsla Int Int Int Double
           | ChslaPt Pt Pt Pt Double


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
data Percentage = Percentage Double
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
