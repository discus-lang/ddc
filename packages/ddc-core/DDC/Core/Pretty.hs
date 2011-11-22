
-- | Pretty printing for core expressions.
module DDC.Core.Pretty 
        ( module DDC.Type.Pretty
        , module DDC.Base.Pretty)
where
import DDC.Core.Exp
import DDC.Type.Pretty
import DDC.Base.Pretty


-- Witness ----------------------------------------------------------------------------------------
instance Pretty n => Pretty (Witness n) where
 pprPrec d ww
  = case ww of
        WCon wc         -> ppr wc
        WVar n          -> ppr n

        WApp w1 w2
         -> pprParen (d > 10) (ppr w1 <+> pprPrec 11 w2)
         
        WJoin w1 w2
         -> pprParen (d > 9)  (ppr w1 <+> text "<>" <+> ppr w2)


instance Pretty WiCon where
 ppr wc
  = case wc of
        WiConPure       -> text "pure"
        WiConEmpty      -> text "empty"
        WiConConst      -> text "const"
        WiConMutable    -> text "mutable"
        WiConLazy       -> text "lazy"
        WiConDirect     -> text "direct"
        WiConRead       -> text "read"
        WiConFree       -> text "free"
        WiConDistinct n
         -> text "distinct" <> (text $ show n)


