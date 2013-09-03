
module DDC.Core.Lite.Compounds
        ( tBoolU, tBool
        , tNatU,  tNat, dcNatU, xNatU
        , tIntU,  tInt
        , tWordU
        , tPair
        , tList)
where
import DDC.Core.Lite.Name
import DDC.Core.Compounds
import DDC.Core.Exp


-- Bools ----------------------------------------------------------------------
-- | Application of the Bool type constructor.
tBool  :: Region Name -> Type Name
tBool r1 
 = TApp (TCon tcBool) r1
 where  tcBool  = TyConBound (UPrim (NameDataTyCon DataTyConBool) kBool) kBool
        kBool   = kFun kRegion kData


-- | Unboxed `Bool#` type constructor.
tBoolU :: Type Name
tBoolU  = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConBool) kData) kData)


-- Nats -----------------------------------------------------------------------
-- | The Nat# type constructor.
tNatU ::  Type Name
tNatU   = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConNat) kData) kData)


-- | A Literal Nat# data constructor.
dcNatU :: Integer -> DaCon Name
dcNatU i = DaConPrim (NameLitNat i) tNatU


-- | A literal Nat#
xNatU  :: a -> Integer -> Exp a Name
xNatU a i = XCon a (dcNatU i)


-- | Application of the Nat type constructor.
tNat :: Region Name -> Type Name
tNat r1 
 = TApp (TCon tcNat) r1
 where  tcNat   = TyConBound (UPrim (NameDataTyCon DataTyConNat) kNat) kNat
        kNat    = kFun kRegion kData


-- Ints -----------------------------------------------------------------------
-- | Unboxed `Int#` type constructor.
tIntU ::  Type Name
tIntU   = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConInt) kData) kData)


-- | Application of the Int type constructor.
tInt :: Region Name -> Type Name
tInt r1 
 = TApp (TCon tcInt) r1
 where  tcInt   = TyConBound (UPrim (NameDataTyCon DataTyConInt) kInt) kInt
        kInt    = kFun kRegion kData


-- Words ----------------------------------------------------------------------
-- | Unboxed `WordN#` type constructor of the given width.
tWordU :: Int -> Type Name
tWordU bits 
 = TCon (TyConBound (UPrim (NamePrimTyCon (PrimTyConWord bits)) kData) kData)


-- Pairs ----------------------------------------------------------------------
-- | Application of the Pair type constructor.
tPair :: Region Name -> Type Name -> Type Name -> Type Name
tPair tR tA tB
 = tApps (TCon tcPair) [tR, tA, tB]
 where  tcPair  = TyConBound (UPrim (NameDataTyCon DataTyConPair) kPair) kPair
        kPair   = kFuns [kRegion, kData, kData] kData


-- Lists ----------------------------------------------------------------------
-- | Application of the List type constructor.
tList :: Region Name -> Type Name -> Type Name
tList tR tA
 = tApps (TCon tcList) [tR, tA]
 where  tcList  = TyConBound (UPrim (NameDataTyCon DataTyConList) kList) kList
        kList   = kRegion `kFun` kData `kFun` kData

