
-- | Bindings to functions exported by the runtime system,
--   and wrappers for related primops.
module DDC.Core.Salt.Runtime
        ( -- * Runtime Config
          Config  (..)
        , runtimeImportKinds
        , runtimeImportTypes

          -- * Types defined in the runtime system.
        , rTop

          -- * Functions defined in the runtime system.
        , xGetTag

        , xAllocBoxed
        , xGetFieldOfBoxed
        , xSetFieldOfBoxed

        , xAllocRawSmall
        , xPayloadOfRawSmall

        , xAllocThunk
        , xAvailOfThunk
        , xSetFieldOfThunk
        , xExtendThunk
        , xCopyAvailOfThunk
        , xApplyThunk

          -- * Calls to primops.
        , xCreate
        , xRead
        , xWrite
        , xPeekBuffer
        , xPokeBuffer
        , xFail
        , xReturn)
where
import DDC.Core.Salt.Compounds
import DDC.Core.Salt.Name
import DDC.Core.Salt.Env
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Base.Pretty
import qualified Data.Map       as Map
import Data.Map                 (Map)


-- Runtime --------------------------------------------------------------------
-- | Runtime system configuration
data Config
        = Config
        { -- | Used a fixed-size heap of this many bytes.
          configHeapSize        :: Integer 
        }


-- | Kind signatures for runtime types that we use when converting to Salt.
runtimeImportKinds :: Map Name (ImportSource Name)
runtimeImportKinds
 = Map.fromList
   [ rn ukTop ]
 where   rn (UName n, t)  = (n, ImportSourceModule (ModuleName ["Runtime"]) n t)
         rn _   = error "ddc-core-salt: all runtime bindings must be named."


-- | Type signatures for runtime funtions that we use when converting to Salt.
runtimeImportTypes :: Map Name (ImportSource Name)
runtimeImportTypes
 = Map.fromList 
   [ rn utGetTag
   , rn utAllocBoxed
   , rn utGetFieldOfBoxed
   , rn utSetFieldOfBoxed
   , rn utAllocRawSmall
   , rn utPayloadOfRawSmall 
   , rn utAllocThunk 
   , rn utAvailOfThunk
   , rn utSetFieldOfThunk
   , rn utExtendThunk
   , rn utCopyAvailOfThunk 
   , rn (utApplyThunk 0)
   , rn (utApplyThunk 1)
   , rn (utApplyThunk 2)
   , rn (utApplyThunk 3)
   , rn (utApplyThunk 4) ]

 where   rn (UName n, t)  = (n, ImportSourceSea (renderPlain $ ppr n) t)
         rn _   = error "ddc-core-salt: all runtime bindings must be named."


-- Tags -----------------------------------------------------------------------
-- | Get the constructor tag of an object.
xGetTag :: a -> Type Name -> Exp a Name -> Exp a Name
xGetTag a tR x2 
 = xApps a (XVar a $ fst utGetTag)
        [ XType a tR, x2 ]

utGetTag :: (Bound Name, Type Name)
utGetTag 
 =      ( UName (NameVar "getTag")
        ,       tForall kRegion $ \r -> tPtr r tObj `tFunPE` tTag)


-- Boxed ----------------------------------------------------------------------
-- | Allocate a Boxed object.
xAllocBoxed :: a -> Type Name -> Integer -> Exp a Name -> Exp a Name
xAllocBoxed a tR tag x2
 = xApps a (XVar a $ fst utAllocBoxed)
        [ XType a tR
        , XCon a (DaConPrim (NameLitTag tag) tTag)
        , x2]

utAllocBoxed :: (Bound Name, Type Name)
utAllocBoxed
 =      ( UName (NameVar "allocBoxed")
        , tForall kRegion $ \r -> (tTag `tFunPE` tNat `tFunPE` tPtr r tObj))


-- | Get a field of a Boxed object.
xGetFieldOfBoxed 
        :: a 
        -> Type Name    -- ^ Prime region var of object.
        -> Type Name    -- ^ Type of field object
        -> Exp a Name   -- ^ Object to update.
        -> Integer      -- ^ Field index.
        -> Exp a Name

xGetFieldOfBoxed a trPrime tField x2 offset
 = xApps a (XVar a $ fst utGetFieldOfBoxed) 
        [ XType a trPrime, XType a tField
        , x2
        , xNat a offset ]

utGetFieldOfBoxed :: (Bound Name, Type Name)
utGetFieldOfBoxed 
 =      ( UName (NameVar "getFieldOfBoxed")
        , tForalls [kRegion, kData]
                $ \[r1, t2] 
                -> tPtr r1 tObj
                        `tFunPE` tNat 
                        `tFunPE` t2)


-- | Set a field in a Boxed Object.
xSetFieldOfBoxed 
        :: a 
        -> Type Name    -- ^ Prime region var of object.
        -> Type Name    -- ^ Region of field object.
        -> Exp a Name   -- ^ Object to update.
        -> Integer      -- ^ Field index.
        -> Exp a Name   -- ^ New field value.
        -> Exp a Name

xSetFieldOfBoxed a trPrime tField x2 offset val
 = xApps a (XVar a $ fst utSetFieldOfBoxed) 
        [ XType a trPrime, XType a tField
        , x2
        , xNat a offset
        , val]

utSetFieldOfBoxed :: (Bound Name, Type Name)
utSetFieldOfBoxed 
 =      ( UName (NameVar "setFieldOfBoxed")
        , tForalls [kRegion, kData]
            $ \[r1, t2] -> tPtr r1 tObj `tFunPE` tNat `tFunPE` t2 `tFunPE` tVoid)


-- RawSmall -------------------------------------------------------------------
-- | Allocate a RawSmall object.
xAllocRawSmall :: a -> Type Name -> Integer -> Exp a Name -> Exp a Name
xAllocRawSmall a tR tag x2
 = xApps a (XVar a $ fst utAllocRawSmall)
        [ XType a tR, xTag a tag, x2]

utAllocRawSmall :: (Bound Name, Type Name)
utAllocRawSmall
 =      ( UName (NameVar "allocRawSmall")
        , tForall kRegion $ \r -> (tTag `tFunPE` tNat `tFunPE` tPtr r tObj))


-- | Get the payload of a RawSmall object.
xPayloadOfRawSmall :: a -> Type Name -> Exp a Name -> Exp a Name
xPayloadOfRawSmall a tR x2 
 = xApps a (XVar a $ fst utPayloadOfRawSmall) 
        [XType a tR, x2]
 
utPayloadOfRawSmall :: (Bound Name, Type Name)
utPayloadOfRawSmall
 =      ( UName (NameVar "payloadOfRawSmall")
        , tForall kRegion $ \r -> (tFunPE (tPtr r tObj) (tPtr r (tWord 8))))


-- Thunk ----------------------------------------------------------------------
-- | Allocate a Thunk object.
xAllocThunk  
        :: a -> Type Name 
        -> Exp a Name -> Exp a Name -> Exp a Name -> Exp a Name

xAllocThunk a tR xFun xArity xAvail
 = xApps a (XVar a $ fst utAllocThunk)
        [ XType a tR, xFun, xArity, xAvail]

utAllocThunk :: (Bound Name, Type Name)
utAllocThunk
 =      ( UName (NameVar "allocThunk")
        , tForall kRegion 
           $ \tR -> (tAddr `tFunPE` tNat `tFunPE` tNat `tFunPE` tPtr tR tObj))


-- | Get the available arguments in a thunk.
xAvailOfThunk
        :: a -> Type Name
        -> Exp a Name -> Exp a Name

xAvailOfThunk a tR xThunk
 = xApps a (XVar a $ fst utAvailOfThunk)
        [ XType a tR, xThunk ]

utAvailOfThunk :: (Bound Name, Type Name)
utAvailOfThunk
 =      ( UName (NameVar "availOfThunk")
        , tForall kRegion
           $ \tR -> (tPtr tR tObj `tFunPE` tNat))


-- | Set one of the argument pointers in a thunk.
xSetFieldOfThunk 
        :: a -> Type Name 
        -> Exp a Name -> Exp a Name -> Exp a Name -> Exp a Name -> Exp a Name

xSetFieldOfThunk a tR xObj xBase xIndex xVal
 = xApps a (XVar a $ fst utSetFieldOfThunk)
        [ XType a tR, xObj, xBase, xIndex, xVal]

utSetFieldOfThunk :: (Bound Name, Type Name)
utSetFieldOfThunk
 =      ( UName (NameVar "setFieldOfThunk")
        , tForall kRegion 
           $ \tR -> (tPtr tR tObj 
                        `tFunPE` tNat  `tFunPE` tNat 
                        `tFunPE` tAddr `tFunPE` tVoid))


-- | Copy a thunk while extending the number of available argument slots.
xExtendThunk
        :: a -> Type Name -> Type Name
        -> Exp a Name -> Exp a Name -> Exp a Name

xExtendThunk a tRSrc tRDst xSrc xMore
 = xApps a (XVar a $ fst utExtendThunk)
        [ XType a tRSrc, XType a tRDst, xSrc, xMore ]

utExtendThunk :: (Bound Name, Type Name)
utExtendThunk
 =      ( UName (NameVar "extendThunk")
        , tForalls [kRegion, kRegion]
           $ \[tR1, tR2] -> (tPtr tR1 tObj `tFunPE` tNat `tFunPE` tPtr tR2 tObj))



-- | Copy the available arguments from one thunk to another.
xCopyAvailOfThunk
        :: a -> Type Name -> Type Name
        -> Exp a Name -> Exp a Name -> Exp a Name -> Exp a Name -> Exp a Name

xCopyAvailOfThunk a tRSrc tRDst xSrc xDst xIndex xLen
 = xApps a (XVar a $ fst utCopyAvailOfThunk)
        [ XType a tRSrc, XType a tRDst, xSrc, xDst, xIndex, xLen ]


utCopyAvailOfThunk :: (Bound Name, Type Name)
utCopyAvailOfThunk
 =      ( UName (NameVar "copyAvailOfThunk")
        , tForalls [kRegion, kRegion]
           $ \[tR1, tR2] -> (tPtr tR1 tObj 
                                `tFunPE` tPtr tR2 tObj
                                `tFunPE` tNat `tFunPE` tNat 
                                `tFunPE` tPtr tR2 tObj))


-- | Apply a thunk to some more arguments.
xApplyThunk
        :: a -> Int
        -> [Exp a Name] -> Exp a Name

xApplyThunk a arity xsArgs
 = xApps a (XVar a $ fst (utApplyThunk arity)) xsArgs


utApplyThunk :: Int -> (Bound Name, Type Name)
utApplyThunk arity
 = let  Just t = tFunOfListPE ([tAddr] ++ replicate arity tAddr ++ [tAddr])
   in   ( UName (NameVar $ "apply" ++ show arity)
        , t )


-- Primops --------------------------------------------------------------------
-- | Create the heap.
xCreate :: a -> Integer -> Exp a Name
xCreate a bytes
        = XApp a (XVar a uCreate) 
                 (xNat  a bytes) 

uCreate :: Bound Name
uCreate = UPrim (NamePrimOp $ PrimStore $ PrimStoreCreate)
                (tNat `tFunPE` tVoid)


-- | Read a value from an address plus offset.
xRead   :: a -> Type Name -> Exp a Name -> Integer -> Exp a Name
xRead a tField xAddr offset
        = XApp a (XApp a (XApp a (XVar a uRead) 
                               (XType a tField))
                          xAddr)
                 (xNat a offset)

uRead   :: Bound Name
uRead   = UPrim (NamePrimOp $ PrimStore $ PrimStoreRead)
                (tForall kData $ \t -> tAddr `tFunPE` tNat `tFunPE` t)


-- | Write a value to an address plus offset.
xWrite   :: a -> Type Name -> Exp a Name -> Integer -> Exp a Name -> Exp a Name
xWrite a tField xAddr offset xVal
        = XApp a (XApp a (XApp a (XApp a (XVar a uWrite) 
                                         (XType a tField))
                                  xAddr)
                          (xNat a offset))
                  xVal

uWrite   :: Bound Name
uWrite   = UPrim (NamePrimOp $ PrimStore $ PrimStoreWrite)
                 (tForall kData $ \t -> tAddr `tFunPE` tNat `tFunPE` t `tFunPE` tVoid)


-- | Peek a value from a buffer pointer plus offset
xPeekBuffer :: a -> Type Name -> Type Name -> Exp a Name -> Integer -> Exp a Name
xPeekBuffer a r t xPtr offset
 = let castedPtr = xCast a r t (tWord 8) xPtr
   in  XApp a (XApp a (XApp a (XApp a (XVar a uPeek) 
                                      (XType a r)) 
                              (XType a t)) 
                       castedPtr) 
              (xNat a offset)

uPeek :: Bound Name
uPeek = UPrim (NamePrimOp $ PrimStore $ PrimStorePeek)
              (typeOfPrimStore PrimStorePeek)
              

-- | Poke a value from a buffer pointer plus offset
xPokeBuffer :: a -> Type Name -> Type Name -> Exp a Name -> Integer -> Exp a Name -> Exp a Name
xPokeBuffer a r t xPtr offset xVal
 = let castedPtr = xCast a r t (tWord 8) xPtr
   in  XApp a (XApp a (XApp a (XApp a (XApp a (XVar a uPoke) 
                                              (XType a r)) 
                                      (XType a t)) 
                               castedPtr) 
                      (xNat a offset))
              xVal

uPoke :: Bound Name
uPoke = UPrim (NamePrimOp $ PrimStore $ PrimStorePoke)
              (typeOfPrimStore PrimStorePoke)


-- | Cast a pointer
xCast :: a -> Type Name -> Type Name -> Type Name -> Exp a Name -> Exp a Name
xCast a r toType fromType xPtr
 =     XApp a (XApp a (XApp a (XApp a (XVar a uCast)
                                      (XType a r)) 
                              (XType a toType))
                      (XType a fromType))
              xPtr           
                      
uCast :: Bound Name
uCast = UPrim (NamePrimOp $ PrimStore $ PrimStoreCastPtr)
              (typeOfPrimStore PrimStoreCastPtr)
              
                             
-- | Fail with an internal error.
xFail   :: a -> Type Name -> Exp a Name
xFail a t       
 = XApp a (XVar a uFail) (XType a t)
 where  uFail   = UPrim (NamePrimOp (PrimControl PrimControlFail)) tFail
        tFail   = TForall (BAnon kData) (TVar $ UIx 0)


-- | Return a value.
--   like  (return# [Int32#] x)
xReturn :: a -> Type Name -> Exp a Name -> Exp a Name
xReturn a t x
 = XApp a (XApp a (XVar a (UPrim (NamePrimOp (PrimControl PrimControlReturn))
                          (tForall kData $ \t1 -> t1 `tFunPE` t1)))
                (XType a t))
           x

