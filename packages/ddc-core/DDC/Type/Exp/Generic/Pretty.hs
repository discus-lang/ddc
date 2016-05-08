{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module DDC.Type.Exp.Generic.Pretty
        ( PrettyConfig
        , pprRawT
        , pprRawPrecT
        , pprRawC)
where
import DDC.Type.Exp.Generic.Exp
import DDC.Base.Pretty


-- | Synonym for pretty constraints on the configurable types.
type PrettyConfig l
      = ( Pretty (GAnnot   l)
        , Pretty (GBindVar l), Pretty (GBoundVar l)
        , Pretty (GBindCon l), Pretty (GBoundCon l)
        , Pretty (GPrim    l))


-- | Pretty print a type using the generic, raw syntax.
pprRawT     :: PrettyConfig l => GType l -> Doc
pprRawT tt = pprRawPrecT 0 tt


-- | Like `pprRawT`, but take the initial precedence.
pprRawPrecT :: PrettyConfig l => Int -> GType l -> Doc
pprRawPrecT d tt
 = case tt of
        TAnnot a t
         ->  braces (ppr a) 
         <+> pprRawT t

        TCon c   -> pprRawC c
        TVar u   -> ppr u

        TAbs b t 
         -> pprParen (d > 1) 
         $  text "λ" <> ppr b <> text "." <+> pprRawT t

        TApp t1 t2
         -> pprParen (d > 10)
         $  pprRawT t1 <+> pprRawPrecT 11 t2


-- | Pretty print a type constructor using the generic, raw syntax.
pprRawC :: PrettyConfig l => GTyCon l -> Doc
pprRawC cc
  = case cc of
        TyConFun        -> text "→"
        TyConUnit       -> text "1"
        TyConVoid       -> text "0"
        TyConSum    k n -> text "Σ" <> braces (pprRawT k <> comma <+> ppr n)
        TyConBot    k   -> text "⊥" <> braces (pprRawT k)
        TyConForall k   -> text "∀" <> braces (pprRawT k)
        TyConExists k   -> text "∃" <> braces (pprRawT k)
        TyConPrim   p   -> ppr p
        TyConBound  u   -> ppr u

