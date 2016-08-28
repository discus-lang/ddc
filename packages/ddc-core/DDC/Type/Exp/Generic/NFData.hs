{-# LANGUAGE UndecidableInstances #-}

module DDC.Type.Exp.Generic.NFData where
import DDC.Type.Exp.Generic.Exp
import Control.DeepSeq


type NFDataLanguage l
        = ( NFData l
          , NFData (GTAnnot l)
          , NFData (GTBindVar l), NFData (GTBoundVar l)
          , NFData (GTBindCon l), NFData (GTBoundCon l)
          , NFData (GTPrim l))


instance NFDataLanguage l => NFData (GType l) where
 rnf xx
  = case xx of
        TAnnot a t      -> rnf a  `seq` rnf t
        TCon   tc       -> rnf tc
        TVar   bv       -> rnf bv
        TAbs   bv k t   -> rnf bv `seq` rnf k `seq` rnf t
        TApp   t1 t2    -> rnf t1 `seq` rnf t2


instance NFDataLanguage l => NFData (GTyCon l) where
 rnf xx
  = case xx of
        TyConUnit       -> ()
        TyConVoid       -> ()
        TyConFun        -> ()
        TyConUnion  t   -> rnf t
        TyConBot    t   -> rnf t
        TyConForall t   -> rnf t
        TyConExists t   -> rnf t
        TyConPrim   p   -> rnf p
        TyConBound  bc  -> rnf bc

