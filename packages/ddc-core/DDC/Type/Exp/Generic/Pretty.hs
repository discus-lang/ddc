{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module DDC.Type.Exp.Generic.Pretty where
import DDC.Type.Exp.Generic.Exp
import DDC.Base.Pretty


-- | Synonym for pretty constraints on all language types.
type PrettyLanguage l
      = ( Eq l
        , Pretty (GAnnot l)
        , Pretty (GBind  l), Pretty (GBound l)
        , Pretty (GCon   l), Pretty (GPrim  l))


instance PrettyLanguage l => Pretty (GType l) where
 pprPrec d tt
  = case tt of
        TAnnot a x
         ->  braces (ppr a) 
         <+> ppr x

        TCon c   -> ppr c
        TVar u   -> ppr u

        TAbs b t 
         -> pprParen (d > 1) 
         $  text "λ" <> ppr b <> text "." <+> ppr t

        TApp t1 t2
         -> pprParen (d > 10)
         $  ppr t1 <+> pprPrec 11 t2


instance PrettyLanguage l => Pretty (GCon l) where
 ppr cc
  = case cc of
        TConArr         -> text "(->)"
        TConPrim   p    -> ppr p
        TConSum    k n  -> text "Σ" <> braces (ppr k <> comma <+> ppr n)
        TConZero   k    -> text "0" <> braces (ppr k)
        TConAll    k    -> text "∀" <> braces (ppr k)
        TConExists k    -> text "∃" <> braces (ppr k)

