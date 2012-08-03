
module DDC.Core.Lite.Compounds
        ( tBoolU
        , tNatU, tIntU
        , tWordU

        , tUnit
        , tBool
        , tNat,  tInt
        , tPair
        , tList)
where
import DDC.Core.Lite.Name
import DDC.Core.Salt.Name.Prim
import DDC.Type.Exp
import DDC.Type.Compounds


-- Type -----------------------------------------------------------------------
tBoolU :: Type Name
tBoolU  = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConBool) kData) kData)


tNatU ::  Type Name
tNatU   = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConNat) kData) kData)


tIntU ::  Type Name
tIntU   = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConInt) kData) kData)


tWordU :: Int -> Type Name
tWordU bits = TCon (TyConBound (UPrim (NamePrimTyCon (PrimTyConWord bits)) kData) kData)


-- | Application of the Unit type constructor.
tUnit :: Type Name
tUnit   = TCon $ TyConBound (UPrim (NameDataTyCon DataTyConUnit) kData) kData


-- | Application of the Bool type constructor.
tBool  :: Region Name -> Type Name
tBool r1 
 = TApp (TCon tcBool) r1
 where  tcBool  = TyConBound (UPrim (NameDataTyCon DataTyConBool) kBool) kBool
        kBool   = kFun kRegion kData

-- | Application of the Nat type constructor.
tNat :: Region Name -> Type Name
tNat r1 
 = TApp (TCon tcNat) r1
 where  tcNat   = TyConBound (UPrim (NameDataTyCon DataTyConNat) kNat) kNat
        kNat    = kFun kRegion kData


-- | Application of the Int type constructor.
tInt :: Region Name -> Type Name
tInt r1 
 = TApp (TCon tcInt) r1
 where  tcInt   = TyConBound (UPrim (NameDataTyCon DataTyConInt) kInt) kInt
        kInt    = kFun kRegion kData


-- | Application of the Pair type constructor.
tPair :: Region Name -> Type Name -> Type Name -> Type Name
tPair tR tA tB
 = tApps (TCon tcPair) [tR, tA, tB]
 where  tcPair  = TyConBound (UPrim (NameDataTyCon DataTyConPair) kPair) kPair
        kPair   = kFuns [kRegion, kData, kData] kData


-- | Application of the List type constructor.
tList :: Region Name -> Type Name -> Type Name
tList tR tA
 = tApps (TCon tcList) [tR, tA]
 where  tcList  = TyConBound (UPrim (NameDataTyCon DataTyConList) kList) kList
        kList   = kRegion `kFun` kData `kFun` kData

