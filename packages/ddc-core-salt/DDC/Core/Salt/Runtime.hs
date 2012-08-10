
-- | Bindings to functions exported by the runtime system,
--   and wrappers for related primops.
module DDC.Core.Salt.Runtime
        ( -- * Runtime Config
          Config  (..)
        , runtimeImportKinds
        , runtimeImportTypes

          -- * Types defined in the runtime system.
        , tTop

          -- * Functions defined in the runtime system.
        , xGetTag
        , xAllocBoxed
        , xGetFieldOfBoxed
        , xSetFieldOfBoxed
        , xAllocRawSmall
        , xPayloadOfRawSmall

          -- * Calls to primops.
        , xCreate
        , xRead
        , xWrite
        , xFail
        , xReturn)
where
import DDC.Core.Salt.Compounds
import DDC.Core.Salt.Name
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.Compounds
import qualified Data.Map       as Map
import Data.Map                 (Map)


-- Runtime --------------------------------------------------------------------
-- | Runtime system configuration
data Config
        = Config
        { configHeapSize        :: Integer }


-- | Kind signatures for runtime types that we use when converting to Salt.
runtimeImportKinds :: Map Name (QualName Name, Kind Name)
runtimeImportKinds
 = Map.fromList
   [ rn ukTop ]
 where   rn (UName n, t)  = (n, (QualName (ModuleName ["Runtime"]) n, t))
         rn _             = error "runtimeImporKinds: all runtime bindings must be named."


-- | Type signatures for runtime funtions that we use when converting to Salt.
runtimeImportTypes :: Map Name (QualName Name, Type Name)
runtimeImportTypes
 = Map.fromList 
   [ rn utGetTag
   , rn utAllocBoxed
   , rn utGetFieldOfBoxed
   , rn utSetFieldOfBoxed
   , rn utAllocRawSmall
   , rn utPayloadOfRawSmall ]
 where   rn (UName n, t)  = (n, (QualName (ModuleName ["Runtime"]) n, t))
         rn _             = error "runtimeImportTypes: all runtime bindings must be named."


-- Regions ----------------------------
-- | The top level region.
--   This region lives for the whole program, and is used to store objects whose 
--   types don't have region annotations (like function closures and Unit values).
tTop    :: Type Name
tTop    = TCon (TyConBound (fst ukTop) (snd ukTop))

ukTop :: (Bound Name, Kind Name)
ukTop
 =      ( UName (NameVar "rT")
        , kRegion)


-- Tags -------------------------------
-- | Get the constructor tag of an object.
xGetTag :: a -> Type Name -> Exp a Name -> Exp a Name
xGetTag a tR x2 
 = makeXApps a (XVar a $ fst utGetTag)
        [ XType tR, x2 ]

utGetTag :: (Bound Name, Type Name)
utGetTag 
 =      ( UName (NameVar "getTag")
        ,       tForall kRegion $ \r -> tPtr r tObj `tFunPE` tTag)


-- Boxed ------------------------------
-- | Allocate a Boxed object.
xAllocBoxed :: a -> Type Name -> Integer -> Exp a Name -> Exp a Name
xAllocBoxed a tR tag x2
 = makeXApps a (XVar a $ fst utAllocBoxed)
        [XType tR, XCon a (UPrim (NameTag tag) tTag), x2]

utAllocBoxed :: (Bound Name, Type Name)
utAllocBoxed
 =      ( UName (NameVar "allocBoxed")
        ,       tForall kRegion $ \r -> (tTag `tFunPE` tNat `tFunPE` tPtr r tObj))


-- | Get a field of a Boxed object.
xGetFieldOfBoxed 
        :: a 
        -> Type Name    -- ^ Prime region var of object.
        -> Type Name    -- ^ Type of field object.
        -> Exp a Name   -- ^ Object to update.
        -> Integer      -- ^ Field index.
        -> Exp a Name

xGetFieldOfBoxed a trPrime tField x2 offset
 = makeXApps a (XVar a $ fst utGetFieldOfBoxed) 
        [ XType trPrime, XType tField
        , x2, XCon a (UPrim (NameNat offset) tNat)]

utGetFieldOfBoxed :: (Bound Name, Type Name)
utGetFieldOfBoxed 
 =      ( UName (NameVar "getFieldOfBoxed")
        , tForalls [kRegion, kData]
                $ \[r1, t2] -> tPtr r1 tObj `tFunPE`  tNat `tFunPE` t2)


-- | Set a field in a Boxed Object.
xSetFieldOfBoxed 
        :: a 
        -> Type Name    -- ^ Prime region var of object.
        -> Type Name    -- ^ Type of field object.
        -> Exp a Name   -- ^ Object to update.
        -> Integer      -- ^ Field index.
        -> Exp a Name   -- ^ New field value.
        -> Exp a Name

xSetFieldOfBoxed a trPrime tField x2 offset val
 = makeXApps a (XVar a $ fst utSetFieldOfBoxed) 
        [ XType trPrime, XType tField
        , x2, XCon a (UPrim (NameNat offset) tNat), val]

utSetFieldOfBoxed :: (Bound Name, Type Name)
utSetFieldOfBoxed 
 =      ( UName (NameVar "setFieldOfBoxed")
        , tForalls [kRegion, kData]
                $ \[r1, t2] -> tPtr r1 tObj 
                        `tFunPE` tNat 
                        `tFunPE` t2
                        `tFunPE` tVoid)


-- RawSmall ---------------------------
-- | Allocate a RawSmall object.
xAllocRawSmall :: a -> Type Name -> Integer -> Exp a Name -> Exp a Name
xAllocRawSmall a tR tag x2
 = makeXApps a (XVar a $ fst utAllocRawSmall)
        [XType tR, XCon a (UPrim (NameTag tag) tTag), x2]

utAllocRawSmall :: (Bound Name, Type Name)
utAllocRawSmall
 =      ( UName (NameVar "allocRawSmall")
        , tForall kRegion $ \r -> (tTag `tFunPE` tNat `tFunPE` tPtr r tObj))


-- | Get the address of the payload of a RawSmall object.
xPayloadOfRawSmall :: a -> Type Name -> Exp a Name -> Exp a Name
xPayloadOfRawSmall a tR x2 
 = makeXApps a (XVar a $ fst utPayloadOfRawSmall) 
        [XType tR, x2]
 
utPayloadOfRawSmall :: (Bound Name, Type Name)
utPayloadOfRawSmall
 =      ( UName (NameVar "payloadOfRawSmall")
        , tForall kRegion $ \r -> (tFunPE (tPtr r tObj) tAddr))


-- Primops --------------------------------------------------------------------
-- | Create the heap
xCreate :: a -> Integer -> Exp a Name
xCreate a bytes
        = XApp a (XVar a uCreate) 
                 (XCon a (UPrim (NameNat bytes) tNat))

uCreate :: Bound Name
uCreate = UPrim (NamePrim $ PrimStore $ PrimStoreCreate)
                (tNat `tFunPE` tVoid)


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


-- | Write a value to an address plus offset.
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
 = XApp a (XVar a uFail) (XType t)
 where  uFail   = UPrim (NamePrim (PrimControl PrimControlFail)) tFail
        tFail   = TForall (BAnon kData) (TVar $ UIx 0)


-- | Return a value.
--   like  (return# [Int32#] x)
xReturn :: a -> Type Name -> Exp a Name -> Exp a Name
xReturn a t x
 = XApp a (XApp a (XVar a (UPrim (NamePrim (PrimControl PrimControlReturn))
                          (tForall kData $ \t1 -> t1 `tFunPE` t1)))
                (XType t))
           x

