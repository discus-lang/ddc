
module DDC.Base.Pretty
        ( module Text.PrettyPrint.Mainland
        , pprParen
        , vcat)
where
import Text.PrettyPrint.Mainland

-- | Wrap a `Doc` in parens if the predicate is true.
pprParen :: Bool -> Doc -> Doc
pprParen b c
 = if b then parens c
        else c

vcat :: [Doc] -> Doc
vcat    = sep . punctuate line