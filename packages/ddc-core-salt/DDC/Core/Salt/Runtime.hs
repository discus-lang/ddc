
-- | Bindings to functions exported by the runtime system,
--   and wrappers for related primops.
module DDC.Core.Salt.Runtime
        ( -- * PrimOps
          xRead
        , xWrite
        , xFail
        , xReturn

          -- * Runtime
        , runtimeImportSigs
        , xGetTag
        , xAllocBoxed
        , xGetFieldOfBoxed
        , xSetFieldOfBoxed
        , xAllocRawSmall
        , xPayloadOfRawSmall)
where
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Salt.Env
import DDC.Core.Salt.Name
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


-- | Write a value to an address pluss offset.
xWrite   :: a -> Type Name -> Exp a Name -> Integer -> Exp a Name -> Exp a Name
xWrite a tField xAddr offset xVal
        = XApp a (XApp a (XApp a (XApp a (XVar a uWrite) 
                                         (XType tField))
                                  xAddr)
                          (XCon a (UPrim (NameNat offset) tNat)))
                  xVal

uWrite   :: Bound Name
uWrite   = UPrim (NamePrim $ PrimStore $ PrimStoreWrite)
                (tForall kData $ \t -> tAddr `tFunPE` tNat `tFunPE` t `tFunPE` tVoid)


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
   [ rn uGetTag
   , rn uAllocBoxed
   , rn uGetFieldOfBoxed
   , rn uSetFieldOfBoxed
   , rn uAllocRawSmall
   , rn uPayloadOfRawSmall ]
 where   rn (UName n t)  = (n, (QualName (ModuleName ["Runtime"]) n, t))
         rn _            = error "runtimeImportSigs: all runtimed bounds should be named"


-- | Get the constructor tag of an object.
xGetTag :: a -> Exp a Name
xGetTag a = XVar a uGetTag

uGetTag :: Bound Name
uGetTag = UName (NameVar "getTag")
          $ tForall kRegion $ \r -> (tFunPE (tPtr r tObj) tTag)


-- Boxed ------------------------------

-- | Allocate a Boxed object.
xAllocBoxed :: a -> Integer -> Exp a Name -> Exp a Name
xAllocBoxed a tag x2
 = makeXApps a (XVar a uAllocBoxed)
        [XCon a (UPrim (NameTag tag) tTag), x2]

uAllocBoxed :: Bound Name
uAllocBoxed
        = UName (NameVar "allocBoxed")
        $ tForall kRegion $ \r -> (tTag `tFunPE` tNat `tFunPE` tPtr r tObj)


-- | Get a field of a Boxed object.
xGetFieldOfBoxed :: a -> Exp a Name -> Integer -> Exp a Name
xGetFieldOfBoxed a x2 offset
 = makeXApps a (XVar a uGetFieldOfBoxed) 
        [x2, XCon a (UPrim (NameNat offset) tNat)]

uGetFieldOfBoxed :: Bound Name
uGetFieldOfBoxed 
        = UName (NameVar "getFieldOfBoxed")
        $ tForall kRegion $ \r -> (tPtr r tObj `tFunPE` tNat `tFunPE` tPtr r tObj)


-- | Set a field in a Boxed Object.
xSetFieldOfBoxed :: a -> Exp a Name -> Integer -> Exp a Name -> Exp a Name
xSetFieldOfBoxed a x2 offset val
 = makeXApps a (XVar a uSetFieldOfBoxed) 
        [x2, XCon a (UPrim (NameNat offset) tNat), val]

uSetFieldOfBoxed :: Bound Name
uSetFieldOfBoxed 
        = UName (NameVar "setFieldOfBoxed")
        $ tForall kRegion $ \r -> (tPtr r tObj `tFunPE` tNat `tFunPE` tPtr r tObj `tFunPE` tVoid)


-- RawSmall ---------------------------
-- | Allocate a RawSmall object.
xAllocRawSmall :: a -> Type Name -> Integer -> Exp a Name -> Exp a Name
xAllocRawSmall a tR tag x2
 = makeXApps a (XVar a uAllocRawSmall)
        [XType tR, XCon a (UPrim (NameTag tag) tTag), x2]

uAllocRawSmall :: Bound Name
uAllocRawSmall
        = UName (NameVar "allocRawSmall")
        $ tForall kRegion $ \r -> (tTag `tFunPE` tNat `tFunPE` tPtr r tObj)


-- | Get the address of the payload of a RawSmall object.
xPayloadOfRawSmall :: a -> Type Name -> Exp a Name -> Exp a Name
xPayloadOfRawSmall a tR x2 
 = makeXApps a (XVar a uPayloadOfRawSmall) 
        [XType tR, x2]
 
uPayloadOfRawSmall :: Bound Name
uPayloadOfRawSmall
        = UName (NameVar "payloadOfRawSmall")
        $ tForall kRegion $ \r -> (tFunPE (tPtr r tObj) tAddr)

