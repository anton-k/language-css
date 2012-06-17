module GenAttributes where

import Text.PrettyPrint
import Data.List
import Data.Char

gen :: String -> String
gen str = render $ header names $$ body names
    where body = joinFuns . map toFun 
          names = getNames str

getNames :: String -> [String]
getNames = map (filter (/= ',')) . lines


header :: [String] -> Doc
header = wrap . vcat . punctuate comma . map text
    where wrap a = text "-- | Html 4 ++ Html 5 attributes"
                $$ text "module Language.Css.Build.Attributes ("
                $$ a
                $$ text ") where"
                $$ text "import Language.Css.Syntax(Attr)"
                $$ text "import Language.Css.Build(Idents(..))"
                $$ text "import Prelude ()"
                $$ char '\n'

joinFuns :: [Doc] -> Doc
joinFuns = vcat . punctuate (text "\n\n")

toFun :: String -> Doc
toFun str = comment name $$ signature name $$ closure name attrName
    where name = text str
          attrName = text $ filter isLetter str
          comment a = text "-- | @" <> a <> text "@ attribute"
          signature a = a <> text " :: Attr" 
          closure name attrName = name <+> text "= ident" 
                                <+> doubleQuotes attrName 

main = readFile "Attributes" >>=
    writeFile "Attributes.hs" . gen


