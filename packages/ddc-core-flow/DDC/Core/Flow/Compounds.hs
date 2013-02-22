
module DDC.Core.Flow.Compounds
        ( tBoolU
        , tNatU,  dcNatU, xNatU
        , tIntU
        , tWordU
        , tStream)
where
import DDC.Core.Flow.Name
import DDC.Core.Exp
import DDC.Core.DaCon
import DDC.Type.Compounds


-- Bools ----------------------------------------------------------------------
-- | Unboxed `Bool#` type constructor.
tBoolU :: Type Name
tBoolU  = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConBool) kData) kData)


-- Nats -----------------------------------------------------------------------
-- | The Nat# type constructor.
tNatU ::  Type Name
tNatU   = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConNat) kData) kData)


-- | A Literal Nat# data constructor.
dcNatU :: Integer -> DaCon Name
dcNatU i = mkDaConAlg (NameLitNat i) tNatU


-- | A literal Nat#
xNatU  :: a -> Integer -> Exp a Name
xNatU a i = XCon a (dcNatU i)


-- Ints -----------------------------------------------------------------------
-- | Unboxed `Int#` type constructor.
tIntU ::  Type Name
tIntU   = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConInt) kData) kData)


-- Words ----------------------------------------------------------------------
-- | Unboxed `WordN#` type constructor of the given width.
tWordU :: Int -> Type Name
tWordU bits 
 = TCon (TyConBound (UPrim (NamePrimTyCon (PrimTyConWord bits)) kData) kData)


-- Streams --------------------------------------------------------------------
tStream :: Type Name -> Type Name -> Type Name -> Type Name
tStream tR tK tA
 = tApps (TCon tcStream) [tR, tK, tA]
 where  uStream         = UPrim (NameDataTyCon DataTyConStream) kStream
        tcStream        = TyConBound uStream kStream
        kStream         = kRegion `kFun` kClosure `kFun` kData `kFun` kData
