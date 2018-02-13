{-# LANGUAGE TypeFamilies #-}
module DDC.Source.Discus.Prim.TyCon
        ( pattern KData
        , pattern KRegion
        , pattern KEffect

        , pattern TImpl
        , pattern TSusp
        , pattern TRead
        , pattern TWrite
        , pattern TAlloc)
where
import DDC.Source.Discus.Prim.Base
import DDC.Type.Exp.Generic.Exp
import DDC.Type.Exp.TyCon


-- | Representation of the Data kind.
pattern KData           = TCon (TyConPrim (PrimTypeKiCon KiConData))

-- | Representation of the Region kind.
pattern KRegion         = TCon (TyConPrim (PrimTypeKiCon KiConRegion))

-- | Representation of the Effect kind.
pattern KEffect         = TCon (TyConPrim (PrimTypeKiCon KiConEffect))

-- | Representation of an implication type.
pattern TImpl  t1 t2    = TApp (TApp (TCon (TyConPrim (PrimTypeTwCon TwConImpl))) t1) t2

-- | Representation of a suspension type.
pattern TSusp  tE tA    = TApp (TApp (TCon (TyConPrim (PrimTypeTcCon TcConSusp))) tE) tA

-- | Representation of a read effect.
pattern TRead  tR       = TApp (TCon (TyConPrim (PrimTypeTcCon TcConRead)))  tR

-- | Representation of a write effect.
pattern TWrite tR       = TApp (TCon (TyConPrim (PrimTypeTcCon TcConWrite))) tR

-- | Representation of a alloc effect.
pattern TAlloc tR       = TApp (TCon (TyConPrim (PrimTypeTcCon TcConAlloc))) tR

