
-- | Pretty printing for core expressions.
module DDC.Core.Pretty 
        ( module DDC.Type.Pretty
        , module DDC.Base.Pretty)
where
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Type.Pretty
import DDC.Type.Compounds
import DDC.Base.Pretty


-- Binder -----------------------------------------------------------------------------------------
-- | Pretty print a binder, adding spaces after names.
--   The RAnon and None binders don't need spaces, as they're single symbols.
pprBinderSep   :: Pretty n => Binder n -> Doc
pprBinderSep bb
 = case bb of
        RName v         -> ppr v <> text " "
        RAnon           -> text "^"
        RNone           -> text "_"


-- | Print a group of binders with the same type.
pprBinderGroup :: (Pretty n, Eq n) => ([Binder n], Type n) -> Doc
pprBinderGroup (rs, t)
        =  text "\\" <> parens ((cat $ map pprBinderSep rs) <> text ":"  <> ppr t) <> dot


-- Exp --------------------------------------------------------------------------------------------
instance (Pretty p, Pretty n, Eq n) => Pretty (Exp a p n) where
 pprPrec d xx
  = case xx of
        XVar  _ n       -> ppr n
        XCon  _ n       -> ppr n
        XPrim _ p       -> ppr p
        
        XLam _ b x      
         | Just (bsMore, xBody) <- takeXLams x
         -> let groups = partitionBindsByType (b:bsMore)
            in  pprParen (d > 1)
                 $ (cat $ map pprBinderGroup groups) <> ppr xBody

         | otherwise 
         -> pprParen (d > 1) 
              $ text "\\" <> parens (ppr b) <> text "." <> ppr x

        XApp _ x1 x2
         -> pprParen (d > 10)
         $  ppr x1 <+> pprPrec 11 x2

        XType    t      -> braces $ ppr t
        XWitness w      -> angles $ ppr w

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
        WiConShare      -> text "share"
        WiConDistinct n
         -> text "distinct" <> (text $ show n)


