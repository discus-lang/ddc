
module DDC.Core.Brine.Lite.Compounds
        ( tBoolU
        , tInt32U

        , tUnit
        , tBool
        , tInt
        , tPair
        , tList)
where
import DDC.Core.Brine.Base.Name
import DDC.Core.Brine.Lite.Name
import DDC.Type.Exp
import DDC.Type.Compounds


-- Type -----------------------------------------------------------------------
tBoolU :: Type Name
tBoolU  = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConBool) kData))

tInt32U :: Type Name
tInt32U = TCon (TyConBound (UPrim (NamePrimTyCon (PrimTyConInt 32)) kData))


-- | Application of the Unit type constructor.
tUnit :: Type Name
tUnit   = TCon (TyConBound (UPrim (NameDataTyCon DataTyConUnit) kData))


-- | Application of the Bool type constructor.
tBool  :: Region Name -> Type Name
tBool r1 = TApp  (TCon (TyConBound (UPrim (NameDataTyCon DataTyConBool)
                                  (kFun kRegion kData))))
                r1


-- | Application of the Int type constructor.
tInt :: Region Name -> Type Name
tInt r1 = TApp  (TCon (TyConBound (UPrim (NameDataTyCon DataTyConInt) 
                                  (kFun kRegion kData))))
                r1

-- | Application of the Pair type constructor.
tPair :: Region Name -> Type Name -> Type Name -> Type Name
tPair tR tA tB
        = tApps (TCon  (TyConBound (UPrim (NameDataTyCon DataTyConPair)
                                          (kFuns [kRegion, kData, kData] kData))))
                [tR, tA, tB]


-- | Application of the List type constructor.
tList :: Region Name -> Type Name -> Type Name
tList tR tA
        = tApps (TCon  (TyConBound (UPrim (NameDataTyCon DataTyConList)
                                          (kRegion `kFun` kData `kFun` kData))))
                [tR, tA]
