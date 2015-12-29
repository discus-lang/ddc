{-# LANGUAGE TypeFamilies #-}

module DDC.Core.Generic.Direct where
import DDC.Core.Generic.Exp


-- | Language AST using direct recursion to the nodes.
data Direct b u c p


instance Language (Direct b u c p) where
 type Bind    (Direct b u c p)  = b
 type Bound   (Direct b u c p)  = u
 type Prim    (Direct b u c p)  = p
 type Type    (Direct b u c p)  = String
 type DaCon   (Direct b u c p)  = c
 type Exp     (Direct b u c p)  = RExp     (Direct b u c p)
 type Lets    (Direct b u c p)  = RLets    (Direct b u c p)
 type Alt     (Direct b u c p)  = RAlt     (Direct b u c p)
 type Pat     (Direct b u c p)  = RPat     (Direct b u c p)
 type Cast    (Direct b u c p)  = RCast    (Direct b u c p)
 type Witness (Direct b u c p)  = RWitness (Direct b u c p)
 type WiCon   (Direct b u c p)  = RWiCon   (Direct b u c p)
