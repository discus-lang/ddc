
module DDC.Core.Sea.Lite.Compounds
        ( tUnit
        , tInt
        , tPair
        , tList)
where
import DDC.Core.Sea.Lite.Name
import DDC.Type.Exp
import DDC.Type.Compounds


-- Type -----------------------------------------------------------------------
-- | Application of the Unit type constructor.
tUnit :: Type Name
tUnit   = TCon (TyConBound (UPrim (NameDataTyCon DataTyConUnit) kData))


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

