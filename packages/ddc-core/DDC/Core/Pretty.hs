
-- | Pretty printing for core expressions.
module DDC.Core.Pretty 
        ( module DDC.Type.Pretty
        , module DDC.Base.Pretty)
where
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Type.Pretty
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Base.Pretty


-- Binder ---------------------------------------------------------------------
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
 =  text "\\"   <> parens ((cat $ map pprBinderSep rs) 
                <> text ":"  <> ppr t) <> dot


-- Exp ------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Exp a n) where
 pprPrec d xx
  = case xx of
        XVar  _ u       -> ppr u
        XCon  _ tc      -> ppr tc
        
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
         $  pprPrec 10 x1 <+> pprPrec 11 x2

        XLet _ lts x
         -> pprParen (d > 2)
         $  ppr lts <+> text "in" <+> ppr x

        XCase _ x alts
         -> pprParen (d > 2) 
         $  text "case" <+> ppr x <+> text "of"
                <+> braces (sep $ punctuate semi $ map ppr alts)

        XCast _ cc x
         -> pprParen (d > 10)
         $  ppr cc <+> pprPrec 11 x

        XType    t      -> text "[" <> ppr t <> text "]"
        XWitness w      -> text "<" <> ppr w <> text ">"



-- Pat ------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Pat n) where
 ppr pp
  = case pp of
        PDefault        -> text "_"
        PData u bs      -> ppr u <+> sep (map pprPatBind bs)


-- | Pretty print a binder, 
--   showing its type annotation only if it's not bottom.
pprPatBind :: (Eq n, Pretty n) => Bind n -> Doc
pprPatBind b
        | isBot (typeOfBind b)  = ppr $ binderOfBind b
        | otherwise             = parens $ ppr b


-- Alt ------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Alt a n) where
 ppr (AAlt p x)         = ppr p <+> text "->" <+> ppr x


-- Cast -----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Cast n) where
 ppr cc
  = case cc of
        CastWeakenEffect  eff   
         -> text "weakeff" <+> parens (ppr eff)

        CastWeakenClosure clo
         -> text "weakclo" <+> parens (ppr clo)

        CastPurify w
         -> text "purify"  <+> text "<" <> ppr w <> text ">"

        CastForget w
         -> text "forget"  <+> text "<" <> ppr w <> text ">"


-- Lets -----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Lets a n) where
 ppr lts
  = case lts of
        LLet m b x
         | isBot $ typeOfBind b 
         -> ppr m       <+> ppr (binderOfBind b)
                        <+> text "="
                        <+> ppr x

         | otherwise
         -> ppr m       <+> ppr b
                        <+> text "="
                        <+> ppr x

        LRec bxs
         -> let pprLetRecBind (b, x)
                 =   ppr (binderOfBind b)
                 <+> text ":"
                 <+> ppr (typeOfBind b)
                 <+> text "="
                 <+> ppr x
        
           in   text "letrec"
                 <+> braces (cat $ punctuate (text "; ") 
                                 $ map pprLetRecBind bxs)


        LLetRegion b []
         -> text "letregion"
                <+> ppr (binderOfBind b)

        LLetRegion b bs
         -> text "letregion"
                <+> ppr (binderOfBind b)
                <+> text "with"
                <+> braces (cat $ punctuate (text "; ") $ map ppr bs)

        LWithRegion b
         -> text "withregion"
                <+> ppr b


instance Pretty LetMode where
 ppr lm
  = case lm of
        LetStrict       -> text "let"
        LetLazy         -> text "laz"


-- Witness --------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Witness n) where
 pprPrec d ww
  = case ww of
        WCon wc         -> ppr wc
        WVar n          -> ppr n

        WApp w1 w2
         -> pprParen (d > 10) (ppr w1 <+> pprPrec 11 w2)
         
        WJoin w1 w2
         -> pprParen (d > 9)  (ppr w1 <+> text "&" <+> ppr w2)

        WType t         -> text "[" <> ppr t <> text "]"


instance Pretty WiCon where
 ppr wc
  = case wc of
        WiConPure       -> text "pure"
        WiConEmpty      -> text "empty"
        WiConGlobal     -> text "global"
        WiConConst      -> text "const"
        WiConMutable    -> text "mutable"
        WiConLazy       -> text "lazy"
        WiConManifest   -> text "manifest"
        WiConUse        -> text "use"
        WiConRead       -> text "read"
        WiConAlloc      -> text "alloc"
        WiConDistinct n -> text "distinct" <> (text $ show n)


