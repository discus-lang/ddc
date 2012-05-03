
module DDC.Core.Salt.Lite.Compounds
        ( tBoolU
        , tIntU
        , tWordU

        , tUnit
        , tBool
        , tInt
        , tPair
        , tList)
where
import DDC.Core.Salt.Base.Name
import DDC.Core.Salt.Lite.Name
import DDC.Type.Exp
import DDC.Type.Compounds


-- Type -----------------------------------------------------------------------
tBoolU :: Type Name
tBoolU  = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConBool) kData))

tIntU :: Int -> Type Name
tIntU bits = TCon (TyConBound (UPrim (NamePrimTyCon (PrimTyConInt bits)) kData))

tWordU :: Int -> Type Name
tWordU bits = TCon (TyConBound (UPrim (NamePrimTyCon (PrimTyConWord bits)) kData))


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


