
-- | Bindings to functions exported by the runtime system,
--   and wrappers for related primops.
module DDC.Core.Salt.Output.Runtime
        ( -- * PrimOps
          xRead
        , xFail
        , xReturn

          -- * Runtime
        , xGetTag
        , xFieldOfBoxed
        , xPayloadOfRawSmall)
where
import DDC.Core.Exp
import DDC.Core.Salt.Output.Env
import DDC.Core.Salt.Output.Name
import DDC.Type.Compounds


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
-- | Get the constructor tag of an object.
xGetTag :: a -> Exp a Name
xGetTag a
 = XVar a (UName (NameVar "getTag")
                 (tFunPE (tPtr tObj) tTag))


-- | Get the address of the payload of a RawSmall object.
xPayloadOfRawSmall :: a -> Exp a Name -> Exp a Name
xPayloadOfRawSmall a x2
 = XApp a (XVar a uPayloadOfRawSmall) x2

uPayloadOfRawSmall :: Bound Name
uPayloadOfRawSmall 
 = UName (NameVar "payloadOfRawSmall")
         (tFunPE (tPtr tObj) tAddr)


-- | Get a field of a Boxed object.
xFieldOfBoxed :: a -> Exp a Name -> Integer -> Exp a Name
xFieldOfBoxed a x2 offset
 = XApp a (XApp a (XVar a uFieldOfBoxed) x2) (XCon a (UPrim (NameNat offset) tNat))

uFieldOfBoxed :: Bound Name
uFieldOfBoxed
 = UName (NameVar "fieldOfBoxed")
         (tPtr tObj `tFunPE` tNat `tFunPE` tPtr tObj)
