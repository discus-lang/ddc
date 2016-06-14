{-# LANGUAGE TypeFamilies #-}
module DDC.Source.Tetra.Prim.TyCon
        ( pattern KData
        , pattern KRegion
        , pattern KEffect

        , pattern TImpl
        , pattern TSusp
        , pattern TRead
        , pattern TWrite
        , pattern TAlloc)
where
import DDC.Source.Tetra.Prim.Base
import DDC.Type.Exp.Generic.Exp
import DDC.Type.Exp.TyCon

pattern KData           = TCon (TyConPrim (PrimTypeKiCon KiConData))
pattern KRegion         = TCon (TyConPrim (PrimTypeKiCon KiConRegion))
pattern KEffect         = TCon (TyConPrim (PrimTypeKiCon KiConEffect))

pattern TImpl  t1 t2    = TApp (TApp (TCon (TyConPrim (PrimTypeTwCon TwConImpl))) t1) t2
pattern TSusp  tE tA    = TApp (TApp (TCon (TyConPrim (PrimTypeTcCon TcConSusp))) tE) tA
pattern TRead  tR       = TApp (TCon (TyConPrim (PrimTypeTcCon TcConRead)))  tR
pattern TWrite tR       = TApp (TCon (TyConPrim (PrimTypeTcCon TcConWrite))) tR
pattern TAlloc tR       = TApp (TCon (TyConPrim (PrimTypeTcCon TcConAlloc))) tR

