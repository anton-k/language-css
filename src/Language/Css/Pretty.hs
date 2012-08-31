module Language.Css.Pretty(
    Pretty(..), prettyPrint)
where

import Text.PrettyPrint
import Language.Css.Syntax


class Pretty a where
    pretty :: a -> Doc

-- | pretty-print with the default style. 
prettyPrint :: Pretty a => a -> String
prettyPrint = render . pretty

ppMaybe :: Pretty a => Maybe a -> Doc
ppMaybe = maybe empty pretty

punctuatePretties :: Pretty a => Doc -> [a] -> Doc
punctuatePretties sep = hcat . punctuate sep . map pretty

vsep = vcat . punctuate (text "\n")

curly :: Doc -> Doc -> Doc
curly head body = (head <+> lbrace) $+$ nest 4 body $$ rbrace

ppBody :: Pretty a => [a] -> Doc
ppBody as = vcat $ punctuate semi $ map pretty as

-- StyleSheet

instance Pretty StyleSheet where
    pretty (StyleSheet as) = vsep $ map pretty as

instance Pretty Rule where
    pretty x = case x of
                SRuleSet    x -> pretty x
                SAtRule vp  x -> char '@' <> ppMaybe vp <> pretty x

-- AtRules

instance Pretty AtRule where
    pretty x = case x of
        AtCharset str               -> ppAtCharSet str
        AtImport head ms            -> ppAtImport head ms
        AtNamespace nspImp impHead  -> ppAtNamespace nspImp impHead
        AtPage pp ds                -> ppAtPage pp ds
        AtFontFace ds               -> ppAtFontFace ds
        AtKeyframes names frames    -> ppAtKeyframes names frames
        AtMedia is ds               -> ppAtMedia is ds

-- @charset
ppAtCharSet str = text "charset " <> text str <+> semi

-- @import
ppAtImport head ms = text "import" <+> pretty head 
    <+> punctuatePretties comma ms <+> semi

instance Pretty ImportHead where
    pretty x = case x of
                IStr x -> text x
                IUri x -> pretty x

-- @namespace
ppAtNamespace namespaceImp impHead = 
    text "namespace" <+> ppMaybe namespaceImp <+> pretty impHead

-- @page
ppAtPage pp ds = text "page" 
    <+> (maybe empty ((colon <>) . pretty)) pp
    <+> (braces $ punctuatePretties semi ds)

-- @media
ppAtMedia ms rs = text "media" 
    <+> punctuatePretties comma ms
    <+> punctuatePretties comma rs
        
-- @font-face
ppAtFontFace ds = text "font-face" 
    <+> (braces $ punctuatePretties semi ds)

-- @keyframes 
ppAtKeyframes name frames = 
    curly (text "keyframes" <+> pretty name)
          (vcat $ map pretty frames)

instance Pretty Frame where 
    pretty (Frame time body) = curly (pretty time) (ppBody body)

instance Pretty FrameTime where
    pretty x = case x of
        From        -> text "from"
        To          -> text "to"
        FrameAt a   -> pretty a

-- RuleSets

instance Pretty RuleSet where
	pretty (RuleSet sels decls) = curly
            (hsep $ punctuate comma $ map pretty sels)
            (ppBody decls)

-- Declarations

instance Pretty Decl where
	pretty (Decl prio p v) = 
            case prio of
                Just x  -> decl <+> pretty x
                Nothing -> decl        
            where decl = pretty p <> char ':' <+> pretty v

instance Pretty Prio where
    pretty = const $ text "!important"

-- Selectors

instance Pretty Sel where
    pretty x = case x of
        SSel a xs       -> ppMaybe a <> (hcat $ map pretty xs)
        CSel comb a b   -> pretty a <> pretty comb <> pretty b

instance Pretty SelComb where
    pretty x = case x of
            Descend     -> space
            Child       -> char '>'
            Adjacent    -> char '+'
            Sibling     -> char '~'

instance Pretty TypeSel where
    pretty (TypeSel a b) = ppMaybe a <> pretty b

instance Pretty ElementSel where
    pretty x = case x of
        UniversalSel    -> char '*'
        ElementSel a    -> pretty a

instance Pretty SimpleSel where
    pretty x = case x of 
        AttributeSel attr   -> pretty attr
        ClassSel a          -> char '.' <> text a
        IdSel a             -> char '#' <> text a
        PseudoSel t val     -> pretty t <> pretty val
        NegationSel a       -> text "not" <> parens (pretty a)


instance Pretty NamespacePrefix where
    pretty x = case x of
        AnyNamespace    -> text "*|"
        BlankNamespace  -> text " |"
        JustNamespace a -> pretty a <> char '|'

instance Pretty PseudoType where
    pretty x = case x of
        OneColon    -> colon
        TwoColons   -> colon <> colon

instance Pretty PseudoVal where
    pretty x = case x of 
                PIdent a -> pretty a
                PFunc  a -> pretty a

instance Pretty NegationArg where
    pretty x = case x of 
        NegationArg1 a  -> pretty a
        NegationArg2 a  -> pretty a

instance Pretty Attr where
    pretty (Attr nsp name rhs) = brackets $ 
        ppMaybe nsp <> pretty name <> ppMaybe rhs

instance Pretty AttrRhs where
    pretty (AttrRhs comb val) = pretty comb <> pretty val

instance Pretty AttrComb where
    pretty x = case x of
        PrefixMatch         -> text "^="
        SuffixMatch         -> text "$="
        SubstringMatch      -> text "*="
        EqualsMatch         -> char '='
        Includes            -> text "~="
        DashMatch           -> text "|="

instance Pretty AttrVal where
    pretty x = case x of
        AttrValIdent a  -> pretty a
        AttrValString a -> doubleQuotes $ text a

instance Pretty Value where
    pretty x = case x of
        VDeg a -> pretty a
        VRad a -> pretty a
        VGrad a -> pretty a
        VColor a -> pretty a
        VHz a -> pretty a
        VKHz a -> pretty a
        VFunc a -> pretty a
        VIdent a -> pretty a
        VInt a -> int a
        VEm a -> pretty a
        VEx a -> pretty a
        VPx a -> pretty a
        VIn a -> pretty a
        VCm a -> pretty a
        VMm a -> pretty a
        VPc a -> pretty a
        VPt a -> pretty a
        VDouble a -> double a
        VPercent a -> pretty a
        VString a -> doubleQuotes $ text a
        VMs a -> pretty a
        VS a -> pretty a
        VUri a -> pretty a
        VNth a -> pretty a

-- Values

instance Pretty Expr where
    pretty x = case x of
                EVal x -> pretty x
                SlashSep x e -> pretty x <> char '/' <> pretty e
                CommaSep x e -> pretty x <> char ',' <+> pretty e
                SpaceSep x e -> pretty x <> space <> pretty e

instance Pretty Func where
    pretty (Func name args) = pretty name 
        <> parens (hsep $ punctuate comma $ fmap pretty args)


instance Pretty Ident where
	pretty (Ident vp a) = ppMaybe vp <> text a


-- Value elems

instance Pretty Deg where
    pretty (Deg x) = double x <> text "deg"

instance Pretty Rad where
    pretty (Rad x) = double x <> text "rad"

instance Pretty Grad where
    pretty (Grad x) = double x <> text "grad"

instance Pretty Color where
    pretty x = case x of 
        Cword a    -> text a 
        Crgb r g b -> col "rgb" $ fmap VInt [r, g, b]
        Crgba r g b a -> col "rgba" $ [VInt r, VInt g, VInt b, VDouble a]
        CrgbPt r g b -> col "rgb" $ fmap VPercent [r, g, b]
        CrgbaPt r g b a -> col "rgba" $ 
            [VPercent r, VPercent g, VPercent b, VDouble a]
        
        Chsl r g b -> col "hsl" $ fmap VInt [r, g, b]
        Chsla r g b a -> col "hsla" $ [VInt r, VInt g, VInt b, VDouble a]
        ChslPt r g b -> col "hsl" $ fmap VPercent [r, g, b]
        ChslaPt r g b a -> col "hsla" $ 
            [VPercent r, VPercent g, VPercent b, VDouble a]
        where col :: String -> [Value] -> Doc
              col name vals = (text name <> ) $  parens $ hsep $
                        punctuate comma $ map pretty vals


instance Pretty Hz where
    pretty (Hz x) = double x <> text "Hz"

instance Pretty KHz where
    pretty (KHz x) = double x <> text "kHz"

instance Pretty Em where
    pretty (Em x) = double x <> text "em"

instance Pretty Ex where
    pretty (Ex x) = double x <> text "ex"

instance Pretty Px where
    pretty (Px x) = int x <> text "px"

instance Pretty In where
    pretty (In x) = double x <> text "in"

instance Pretty Cm where
    pretty (Cm x) = double x <> text "cm"

instance Pretty Mm where
    pretty (Mm x) = double x <> text "mm"

instance Pretty Pc where
    pretty (Pc x) = double x <> text "pc"

instance Pretty Pt where
    pretty (Pt x) = int x <> text "pt"

instance Pretty Percent where
    pretty (Percent x) = double x <> text "%"

instance Pretty Ms where
    pretty (Ms x) = double x <> text "ms"

instance Pretty S where
    pretty (S x) = double x <> text "s"

instance Pretty Uri where
    pretty (Uri x) = text "url" <> (parens $ text x)


instance Pretty Nth where
    pretty x = case x of
        N a b   -> int a <> text "*n+" <> int b
        Nth a   -> int a
        Odd     -> text "odd"
        Even    -> text "even"


-- Vendor prefixes

instance Pretty VendorPrefix where
    pretty x = text $ case x of
        Ms'     -> "-ms-"
        Mso'    -> "-mso-"
        Moz'    -> "-moz-" 
        O'      -> "-o-"
        Xv'     -> "-xv-"
        Atsc'   -> "-atsc-" 
        Wap'    -> "-wap-"
        Khtml'  -> "-khtml-"
        Webkit' -> "-webkit-" 
        Prince' -> "-prince-"
        Ah'     -> "-ah-"
        Hp'     -> "-hp-"
        Ro'     -> "-ro-" 
        Rim'    -> "-rim-"
        Tc'     -> "-tc-"
        VendorMinus a -> '-' : (a ++ "-")
        VendorUnder a -> '_' : (a ++ "-")


