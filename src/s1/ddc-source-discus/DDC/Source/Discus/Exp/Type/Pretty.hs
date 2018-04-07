{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module DDC.Source.Discus.Exp.Type.Pretty
        ( PrettyConfig
        , pprRawT
        , pprRawPrecT
        , pprRawC)
where
import DDC.Source.Discus.Exp.Type.Base
import DDC.Data.Pretty
import qualified Data.Text              as Text


-- | Synonym for pretty constraints on the configurable types.
type PrettyConfig l
      = ( Pretty (GTAnnot   l))


instance Pretty TyConBind where
 ppr (TyConBindName tx)  = text (Text.unpack tx)


instance Pretty TyConBound where
 ppr (TyConBoundName tx) = text (Text.unpack tx)


instance Pretty Bind where
 ppr bb
  = case bb of
        BNone   -> text "_"
        BAnon   -> text "^"
        BName t -> text (Text.unpack t)


instance Pretty Bound where
 ppr uu
  = case uu of
        UIx i   -> int i
        UName t -> text (Text.unpack t)
        UHole   -> text "?"


instance Pretty TyConPrim where
 ppr t
  = case t of
        TyConPrimSoCon c        -> ppr c
        TyConPrimKiCon c        -> ppr c
        TyConPrimTwCon c        -> ppr c
        TyConPrimTcCon c        -> ppr c
        TyConPrimTyCon c        -> ppr c
        TyConPrimDiscus c       -> ppr c


instance Pretty TyConDiscus where
 ppr tc
  = case tc of
        TyConDiscusTuple n   -> text "Tuple" <> int n
        TyConDiscusVector    -> text "Vector"
        TyConDiscusF         -> text "F#"
        TyConDiscusU         -> text "U#"




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

        TAbs b k t
         -> pprParen (d > 1)
         $  text "λ" <> ppr b <> text ":" <+> pprRawT k <> text "." <+> pprRawT t

        TApp t1 t2
         -> pprParen (d > 10)
         $  pprRawT t1 <+> pprRawPrecT 11 t2


-- | Pretty print a type constructor using the generic, raw syntax.
pprRawC :: PrettyConfig l => GTyCon l -> Doc
pprRawC cc
  = case cc of
        TyConFunExplicit -> text "->"
        TyConFunImplicit -> text "~>"
        TyConUnit        -> text "1"
        TyConVoid        -> text "0"
        TyConUnion  k    -> text "∨" <> braces (pprRawT k)
        TyConBot    k    -> text "⊥" <> braces (pprRawT k)
        TyConForall k    -> text "∀" <> braces (pprRawT k)
        TyConExists k    -> text "∃" <> braces (pprRawT k)
        TyConPrim   p    -> ppr p
        TyConBound  u    -> ppr u

