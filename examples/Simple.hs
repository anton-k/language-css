module Main where

import Language.Css.Syntax
import Language.Css.Build
import Language.Css.Pretty
import Language.Css.Build.Idents
import Language.Css.Build.Tags hiding (center)

res = ruleSets [
         body [
             margin =: int 0,
             border =: int 0 ],
            
         h1 [ textAlign =: center],

         p [ 
            backgroundColor =: black, 
            color =: white,
            padding =: spaces [pct 5, pct 5, pct 10, pct 10]  ],

         (star /. "warning") [ color =: red ] 
       ]
 
main = print $ pretty res

