{-# LANGUAGE TypeFamilies #-}

module DDC.Core.Generic.Annot where
import DDC.Core.Generic.Exp


-- | Language AST using annotated expression nodes.
-- ** todo, split into direct module.
data Annot a b u c p

data RAnnot a x = XAnnot a x

instance Language (Annot a b u c p) where
 type Bind    (Annot a b u c p) = b
 type Bound   (Annot a b u c p) = u
 type Prim    (Annot a b u c p) = p
 type Type    (Annot a b u c p) = String
 type DaCon   (Annot a b u c p) = c
 type Exp     (Annot a b u c p) = RAnnot a (RExp (Annot a b u c p))
 type Lets    (Annot a b u c p) = RLets    (Annot a b u c p)
 type Alt     (Annot a b u c p) = RAlt     (Annot a b u c p)
 type Pat     (Annot a b u c p) = RPat     (Annot a b u c p)
 type Cast    (Annot a b u c p) = RCast    (Annot a b u c p)
 type Witness (Annot a b u c p) = RWitness (Annot a b u c p)
 type WiCon   (Annot a b u c p) = RWiCon   (Annot a b u c p)
