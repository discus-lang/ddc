
module DDC.Type.Pretty 
        (module DDC.Base.Pretty)
where
import DDC.Type.Exp
import DDC.Type.Predicates
import DDC.Base.Pretty
import qualified DDC.Type.Sum    as TS


-- Bind, Bound ------------------------------------------------------------------------------
instance Pretty n => Pretty (Bind n) where
 ppr nn
  = case nn of
        BName v t       -> ppr v     <> text ":" <> ppr t
        BAnon   t       -> text "^"  <> text ":" <> ppr t
        BNone   t       -> text "_"  <> text ":" <> ppr t


instance Pretty n => Pretty (Bound n) where
 ppr nn
  = case nn of
        UName v _       -> ppr v
        UIx i _         -> text "^" <> ppr i


-- Type -------------------------------------------------------------------------------------------
instance Pretty n => Pretty (Type n) where
 pprPrec d tt
  = case tt of
        -- Full application of function constructors are printed infix.
        TApp (TApp (TCon (TyConKind KiConFun)) k1) k2
         -> pprParen (d > 5)
         $  ppr k1 <+> text "~>" <+> ppr k2

        TApp (TApp (TCon (TyConWitness TwConImpl)) k1) k2
         -> pprParen (d > 5)
         $  ppr k1 <+> text "=>" <+> pprPrec 6 k2

        TApp (TApp (TApp (TApp (TCon (TyConComp TcConFun)) t1) t2) eff) clo
         | isBottom eff, isBottom clo
         -> pprParen (d > 5)
         $  ppr t1 <+> text "->" <+> pprPrec 6 t2

        -- Standard types.
        TCon tc    -> ppr tc
        TVar b     -> ppr b

        TForall b t
         -> pprParen (d > 1)
         $  brackets (ppr b) <> dot <> ppr t

        TApp t1 t2
         -> pprParen (d > 10)
         $  ppr t1 <+> pprPrec 11 t2

        TSum ts
         -> pprParen (d > 9) 
         $  ppr ts

        TBot k  
         -> ppr k <> text "0"


instance Pretty n => Pretty (TypeSum n) where
 ppr ts
  = sep $ punctuate (text " +") (map ppr $ TS.toList ts)


-- TCon -------------------------------------------------------------------------------------------
instance Pretty n => Pretty (TyCon n) where
 ppr tt
  = case tt of
        TyConSort sc    -> ppr sc
        TyConKind kc    -> ppr kc
        TyConWitness tc -> ppr tc
        TyConComp tc    -> ppr tc


instance Pretty SoCon where
 ppr sc 
  = case sc of
        SoConComp       -> text "**"
        SoConProp       -> text "@@"


instance Pretty KiCon where
 ppr kc
  = case kc of
        KiConFun        -> text "(~>)"
        KiConData       -> text "*"
        KiConRegion     -> text "%"
        KiConEffect     -> text "!"
        KiConClosure    -> text "$"
        KiConWitness    -> text "@"


instance Pretty TwCon where
 ppr tw
  = case tw of
        TwConImpl       -> text "(=>)"
        TwConConst      -> text "Const"
        TwConDeepConst  -> text "DeepConst"
        TwConMutable    -> text "Mutable"
        TwConDeepMutable-> text "DeepMutable"
        TwConLazy       -> text "Lazy"
        TwConHeadLazy   -> text "HeadLazy"
        TwConDirect     -> text "Direct"
        TwConDistinct n -> text "Distinct" <> (text $ show n)
        TwConPure       -> text "Pure"
        TwConEmpty      -> text "Empty"
        

instance Pretty n => Pretty (TcCon n) where
 ppr tc 
  = case tc of
        TcConData v _   -> ppr v
        TcConFun        -> text "(->)"
        TcConRead       -> text "Read"
        TcConDeepRead   -> text "DeepRead"
        TcConWrite      -> text "Write"
        TcConDeepWrite  -> text "DeepWrite"
        TcConAlloc      -> text "Alloc"
        TcConFree       -> text "Free"
        TcConDeepFree   -> text "DeepFree"
