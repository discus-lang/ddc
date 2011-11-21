
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
        BName v k       -> ppr v     <> text ":" <> ppr k
        BAnon   k       -> text "_"  <> text ":" <> ppr k


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
        TApp (TApp (TCon TConKindFun) k1) k2
         -> pprParen (d > 5)
         $  ppr k1 <+> text "~>" <+> ppr k2

        TApp (TApp (TCon (TConType (TyConBuiltin TyConImpl))) k1) k2
         -> pprParen (d > 5)
         $  ppr k1 <+> text "=>" <+> pprPrec 6 k2

        TApp (TApp (TApp (TApp (TCon (TConType (TyConBuiltin TyConFun))) t1) t2) eff) clo
         | isBot eff, isBot clo
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
instance Pretty n => Pretty (TCon n) where
 ppr tt
  = case tt of
        TConSort sc     -> ppr sc
        TConKindFun     -> text "(~>)"
        TConKind kc     -> ppr kc
        TConType tc     -> ppr tc


instance Pretty SoCon where
 ppr sc 
  = case sc of
        SoConComp       -> text "**"
        SoConProp       -> text "@@"


instance Pretty KiCon where
 ppr kc
  = case kc of
        KiConData       -> text "*"
        KiConRegion     -> text "%"
        KiConEffect     -> text "!"
        KiConClosure    -> text "$"
        KiConWitness    -> text "@"

instance Pretty n => Pretty (TyCon n) where
 ppr tc
  = case tc of
        TyConUser v _   -> ppr v
        TyConBuiltin tb -> ppr tb
        
instance Pretty TyConBuiltin where
 ppr tc 
  = case tc of
        TyConFun        -> text "(->)"
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



