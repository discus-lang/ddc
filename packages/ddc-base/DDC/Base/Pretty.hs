
module DDC.Base.Pretty
        ( module Text.PrettyPrint.Mainland
        , pretty
        , pprParen
        , vcat)
where
import Text.PrettyPrint.Mainland                hiding (pretty)
import qualified Text.PrettyPrint.Mainland      as P

-- | Convert a `Doc` to a string, with no wrapping.
-- 
--   TODO: Mainland pretty print doesn't have an API that avoids wrapping, 
--   so we just use a big value for the page width.
pretty :: Doc -> String
pretty doc = P.pretty 1000000 doc

-- | Wrap a `Doc` in parens if the predicate is true.
pprParen :: Bool -> Doc -> Doc
pprParen b c
 = if b then parens c
        else c

-- | Vertically concatenate some documents.
vcat :: [Doc] -> Doc
vcat ds   = folddoc (<>) (punctuate line ds)


instance Pretty () where
 ppr _ = text "()"