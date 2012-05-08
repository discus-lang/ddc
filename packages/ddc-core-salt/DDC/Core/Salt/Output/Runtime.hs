
-- | Bindings to functions exported by the runtime system,
--   and wrappers for related primops.
module DDC.Core.Salt.Output.Runtime
        ( -- * PrimOps
          xRead
        , xFail
        , xReturn

          -- * Runtime
        , runtimeImportSigs
        , xGetTag
        , xFieldOfBoxed
        , xAllocRawSmall
        , xPayloadOfRawSmall)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Salt.Output.Env
import DDC.Core.Salt.Output.Name
import DDC.Type.Compounds
import qualified Data.Map       as Map
import Data.Map                 (Map)

-- Primops --------------------------------------------------------------------
-- | Read a value from an address plus offset.
xRead   :: a -> Type Name -> Exp a Name -> Integer -> Exp a Name
xRead a tField xAddr offset
        = XApp a (XApp a (XApp a (XVar a uRead) 
                               (XType tField))
                          xAddr)
                 (XCon a (UPrim (NameNat offset) tNat))

uRead   :: Bound Name
uRead   = UPrim (NamePrim $ PrimStore $ PrimStoreRead)
                (tForall kData $ \t -> tAddr `tFunPE` tNat `tFunPE` t)


-- | Fail with an internal error.
xFail   :: a -> Type Name -> Exp a Name
xFail a t       
 = XApp a (XVar a (UPrim (NamePrim (PrimControl PrimControlFail))
                         (TForall (BAnon kData) (TVar $ UIx 0 kData))))
          (XType t)


-- | Return a value.
--   like  (return# [Int32#] x)
xReturn :: a -> Type Name -> Exp a Name -> Exp a Name
xReturn a t x
 = XApp a (XApp a (XVar a (UPrim (NamePrim (PrimControl PrimControlReturn))
                          (tForall kData $ \t1 -> t1 `tFunPE` t1)))
                (XType t))
           x


-- Runtime --------------------------------------------------------------------
-- | Signatures for runtime funtions that we use when converting
--   to Disciple Salt code.
runtimeImportSigs :: Map Name (QualName Name, Type Name)
runtimeImportSigs
        = Map.fromList
        [ rn (NameVar "getTag")                 tGetTag
        , rn (NameVar "fieldOfBoxed")           tFieldOfBoxed
        , rn (NameVar "allocRawSmall")          tAllocRawSmall
        , rn (NameVar "payloadOfRawSmall")      tPayloadOfRawSmall ]
        where   rn n t  = (n, (QualName (ModuleName ["Runtime"]) n, t))


-- | Get the constructor tag of an object.
xGetTag :: a -> Exp a Name
xGetTag a
        = XVar a (UName (NameVar "getTag") tGetTag)

tGetTag :: Type Name
tGetTag = tFunPE (tPtr tObj) tTag


-- | Get a field of a Boxed object.
xFieldOfBoxed :: a -> Exp a Name -> Integer -> Exp a Name
xFieldOfBoxed a x2 offset
        = XApp a (XApp a (XVar a u) x2) (XCon a (UPrim (NameNat offset) tNat))
        where u = UName (NameVar "fieldOfBoxed") tFieldOfBoxed

tFieldOfBoxed :: Type Name
tFieldOfBoxed   = tPtr tObj `tFunPE` tNat `tFunPE` tPtr tObj


-- RawSmall ---------------------------
-- | Allocate a RawSmall object.
xAllocRawSmall :: a -> Integer -> Exp a Name -> Exp a Name
xAllocRawSmall a tag x2
        = XApp a (XApp a (XVar a u) (XCon a (UPrim (NameTag tag) tTag))) x2
        where u = UName (NameVar "allocRawSmall") tAllocRawSmall


tAllocRawSmall :: Type Name
tAllocRawSmall
        = tTag `tFunPE` tNat `tFunPE` tPtr tObj


-- | Get the address of the payload of a RawSmall object.
xPayloadOfRawSmall :: a -> Exp a Name -> Exp a Name
xPayloadOfRawSmall a x2 
        = XApp a (XVar a u) x2
        where u = UName (NameVar "payloadOfRawSmall") tPayloadOfRawSmall
 
tPayloadOfRawSmall :: Type Name
tPayloadOfRawSmall
        = tFunPE (tPtr tObj) tAddr



