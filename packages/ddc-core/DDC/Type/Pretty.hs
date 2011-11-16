
module DDC.Type.Pretty where
import DDC.Type.Exp
import DDC.Type.Predicates
import Text.PrettyPrint.Mainland


-- Name, Bind, Bound ------------------------------------------------------------------------------
instance Pretty n => Pretty (Name n) where
 ppr nn
  = case nn of
        NName n         -> ppr n
        NAnon           -> empty
        NIx i           -> brackets $ (text $ show i)


instance Pretty n => Pretty (Bind n) where
 ppr nn
  = case nn of
        BVar  NAnon t   -> ppr t
        BVar  n t       -> parens (ppr n <> text ":" <> ppr t)
        BMore n t c     -> parens (ppr n <> text ":" <> ppr t <> text ":>" <> ppr c)


instance Pretty n => Pretty (Bound n) where
 ppr nn
  = case nn of
        UVar  n _       -> ppr n
        UMore n _ _     -> ppr n


-- Type -------------------------------------------------------------------------------------------
instance Pretty n => Pretty (Type n) where
 pprPrec d tt
  = case tt of
        -- Full application of function constructors are printed infix.
        TApp (TApp (TCon (TConKind KiConFun)) k1) k2
         -> pprParen (d > 5)
         $  ppr k1 <+> text "~>" <+> ppr k2

        TApp (TApp (TCon (TConType TyConImpl)) k1) k2
         -> pprParen (d > 5)
         $  ppr k1 <+> text "=>" <+> ppr k2

        TApp (TApp (TApp (TApp (TCon (TConType TyConFun)) t1) t2) eff) clo
         | isBot eff, isBot clo
         -> pprParen (d > 5)
         $  ppr t1 <+> text "->" <+> ppr t2

        -- Standard types.
        TCon tc    -> ppr tc
        TVar b     -> ppr b

        TLam b t
         -> pprParen (d > 1)
         $  text "\\" <> ppr b <> text "." <> ppr t

        TApp t1 t2
         -> pprParen (d > 10)
         $  ppr t1 <+> pprPrec 11 t2

        TSum t1 t2
         -> pprParen (d > 9) 
         $  ppr t1 <+> text "+" <+> ppr t2

        TBot k  
         -> ppr k <> text "0"


-- TCon -------------------------------------------------------------------------------------------
instance Pretty n => Pretty (TCon n) where
 ppr tt
  = case tt of
        TConSort sc     -> ppr sc
        TConKind kc     -> ppr kc
        TConType tc     -> ppr tc


instance Pretty SoCon where
 ppr sc 
  = case sc of
        SoAny           -> text "??"
        SoComp          -> text "**"
        SoProp          -> text "@@"


instance Pretty KiCon where
 ppr kc
  = case kc of
        KiConAny        -> text "?"
        KiConFun        -> text "(~>)"
        KiConData       -> text "*"
        KiConRegion     -> text "%"
        KiConEffect     -> text "!"
        KiConClosure    -> text "$"
        KiConWitness    -> text "@"


instance Pretty n => Pretty (TyCon n) where
 ppr tc 
  = case tc of
        TyConFun        -> text "(->)"
        TyConData n _   -> ppr n
        TyConRead       -> text "Read"
        TyConDeepRead   -> text "DeepRead"
        TyConWrite      -> text "Write"
        TyConDeepWrite  -> text "DeepWrite"
        TyConAlloc      -> text "Alloc"
        TyConFree       -> text "Free"
        TyConDeepFree   -> text "DeepFree"
        TyConImpl       -> text "(=>)"
        TyConConst      -> text "Const"
        TyConDeepConst  -> text "DeepConst"
        TyConMutable    -> text "Mutable"
        TyConDeepMutable-> text "DeepMutable"
        TyConLazy       -> text "Lazy"
        TyConHeadLazy   -> text "HeadLazy"
        TyConDirect     -> text "Direct"
        TyConDistinct n -> text "Distinct" <> (text $ show n)
        TyConPure       -> text "Pure"
        TyConEmpty      -> text "Empty"


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
        WiConMkPure     -> text "MkPure"
        WiConMkEmpty    -> text "MkEmpty"
        WiConMkConst    -> text "MkConst"
        WiConMkMutable  -> text "MkMutable"
        WiConMkLazy     -> text "MkLazy"
        WiConMkDirect   -> text "MkDirect"
        WiConMkPurify   -> text "MkPurify"
        WiConMkShare    -> text "MkShare"
        WiConMkDistinct n
         -> text "MkDistinct" <> (text $ show n)


-- Utils ------------------------------------------------------------------------------------------
pprParen :: Bool -> Doc -> Doc
pprParen b c
 = if b then parens c
        else c

