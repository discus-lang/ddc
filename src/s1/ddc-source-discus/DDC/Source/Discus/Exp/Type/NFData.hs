{-# OPTIONS_HADDOCK hide #-}

module DDC.Source.Discus.Exp.Type.NFData where
import DDC.Source.Discus.Exp.Type.Base
import Control.DeepSeq


instance NFData Bind where
 rnf !_ = ()


instance NFData Bound where
 rnf !_ = ()


instance NFData TyConBind where
 rnf !_ = ()


instance NFData TyConBound where
 rnf !_ = ()


instance NFData TyConPrim where
 rnf !_ = ()


instance NFData TyConDiscus where
 rnf !_ = ()


instance NFData a => NFData (GType a) where
 rnf xx
  = case xx of
        TAnnot a t      -> rnf a  `seq` rnf t
        TCon tc         -> rnf tc
        TVar _bv        -> ()
        TAbs _bv k t    -> rnf k  `seq` rnf t
        TApp t1 t2      -> rnf t1 `seq` rnf t2
        TRow r          -> rnf r
        TTuple r        -> rnf r
        TRecord r       -> rnf r
        TVariant r      -> rnf r


instance NFData a => NFData (GTyCon a) where
 rnf xx
  = case xx of
        TyConUnit               -> ()
        TyConVoid               -> ()
        TyConFunExplicit        -> ()
        TyConFunImplicit        -> ()
        TyConUnion  t           -> rnf t
        TyConBot    t           -> rnf t
        TyConForall t           -> rnf t
        TyConExists t           -> rnf t
        TyConPrim   p           -> rnf p
        TyConBound  _bc         -> ()

