
-- | Pretty printing for core expressions.
module DDC.Core.Pretty 
        ( module DDC.Type.Pretty
        , module DDC.Base.Pretty)
where
import DDC.Core.Exp
import DDC.Type.Pretty
import DDC.Base.Pretty


-- Exp --------------------------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Exp a p n) where
 pprPrec d xx
  = case xx of
        XVar _ n        -> ppr n
        XCon _ n        -> ppr n
        
        XLam _ u x      -> text "\\" <> parens (ppr u) <> text "." <> ppr x

        XApp _ x1 x2
         -> pprParen (d > 10)
         $  ppr x1 <+> pprPrec 11 x2

        _               -> error "pprPrec[Exp] not finished"


-- Witness ----------------------------------------------------------------------------------------
instance Pretty n => Pretty (Witness n) where
 pprPrec d ww
  = case ww of
        WCon wc         -> ppr wc
        WVar n          -> ppr n

        WApp w1 w2
         -> pprParen (d > 10) (ppr w1 <+> pprPrec 11 w2)
         
        WJoin w1 w2
         -> pprParen (d > 9)  (ppr w1 <+> text " & " <+> ppr w2)


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


