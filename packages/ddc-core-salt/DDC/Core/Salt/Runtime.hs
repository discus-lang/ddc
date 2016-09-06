-- | Bindings to functions exported by the runtime system,
--   and wrappers for related primops.
module DDC.Core.Salt.Runtime
        ( -- * Runtime Config
          Config  (..)
        , runtimeImportKinds
        , runtimeImportTypes

          -- * Runtime Types.
        , rTop

          -- * Runtime Functions
          -- ** Generic
        , xGetTag

          -- ** Boxed Objects
        , xAllocBoxed
        , xGetFieldOfBoxed
        , xSetFieldOfBoxed

          -- ** Raw Objects
        , xAllocRaw
        , xPayloadOfRaw

          -- ** Raw Small Objects
        , xAllocSmall
        , xPayloadOfSmall

          -- ** Thunk Objects
        , xAllocThunk
        , xArgsOfThunk
        , xSetFieldOfThunk
        , xExtendThunk
        , xCopyArgsOfThunk
        , xApplyThunk
        , xRunThunk

          -- ** Error handling
        , xErrorDefault)
where
import DDC.Core.Salt.Compounds
import DDC.Core.Salt.Name
import DDC.Core.Exp.Annot
import DDC.Core.Module
import DDC.Data.Pretty
import qualified Data.Map       as Map
import Data.Map                 (Map)


-- Runtime -----------------------------------------------------------------------------------------
-- | Runtime system configuration
data Config
        = Config
        { -- | Used a fixed-size heap of this many bytes.
          configHeapSize        :: Integer 
        }


-- | Kind signatures for runtime types that we use when converting to Salt.
runtimeImportKinds :: Map Name (ImportType Name (Type Name))
runtimeImportKinds
 = Map.fromList
   [ rn ukTop ]
 where   rn (UName n, t)  = (n, ImportTypeAbstract t)
         rn _   = error "ddc-core-salt: all runtime bindings must be named."


-- | Type signatures for runtime funtions that we use when converting to Salt.
runtimeImportTypes :: Map Name (ImportValue Name (Type Name))
runtimeImportTypes
 = Map.fromList 
   [ rn utGetTag
   , rn utAllocBoxed
   , rn utGetFieldOfBoxed
   , rn utSetFieldOfBoxed

   , rn utAllocSmall
   , rn utPayloadOfSmall 

   , rn utAllocRaw
   , rn utPayloadOfRaw

   , rn utAllocThunk 
   , rn utArgsOfThunk
   , rn utSetFieldOfThunk
   , rn utExtendThunk
   , rn utCopyArgsOfThunk 
   , rn utRunThunk

   , rn (utApplyThunk 0)
   , rn (utApplyThunk 1)
   , rn (utApplyThunk 2)
   , rn (utApplyThunk 3)
   , rn (utApplyThunk 4) 

   , rn utErrorDefault]

 where   rn (UName n, t)  = (n, ImportValueSea (renderPlain $ ppr n) t)
         rn _   = error "ddc-core-salt: all runtime bindings must be named."


-- Regions --------------------------------------------------------------------
-- | The top-level region.
--   This region lives for the whole program, and is used to store objects whose 
--   types don't have region annotations (like function closures and Unit values).
rTop    :: Type Name
rTop   = TVar (fst ukTop)

ukTop :: (Bound Name, Kind Name)
ukTop
 =      ( UName (NameVar "rT")
        , kRegion)


-- Tags -------------------------------------------------------------------------------------------
-- | Get the constructor tag of an object.
xGetTag :: a -> Type Name -> Exp a Name -> Exp a Name
xGetTag a tR x2 
 = xApps a (XVar a $ fst utGetTag)
        [ XType a tR, x2 ]

utGetTag :: (Bound Name, Type Name)
utGetTag 
 =      ( UName (NameVar "getTag")
        ,       tForall kRegion $ \r -> tPtr r tObj `tFun` tTag)


-- Thunk ------------------------------------------------------------------------------------------
-- | Allocate a Thunk object.
xAllocThunk  
        :: a 
        -> Type Name 
        -> Exp a Name   -- ^ Function
        -> Exp a Name   -- ^ Value paramters.
        -> Exp a Name   -- ^ Times boxed.
        -> Exp a Name   -- ^ Value args.
        -> Exp a Name   -- ^ Times run.
        -> Exp a Name

xAllocThunk a tR xFun xParam xBoxes xArgs xRun
 = xApps a (XVar a $ fst utAllocThunk)
        [ XType a tR, xFun, xParam, xBoxes, xArgs, xRun]

utAllocThunk :: (Bound Name, Type Name)
utAllocThunk
 =      ( UName (NameVar "allocThunk")
        , tForall kRegion 
           $ \tR -> (tAddr `tFun` tNat `tFun` tNat `tFun` tNat 
                           `tFun` tNat `tFun` tPtr tR tObj))


-- | Copy the available arguments from one thunk to another.
xCopyArgsOfThunk
        :: a -> Type Name -> Type Name
        -> Exp a Name -> Exp a Name -> Exp a Name -> Exp a Name -> Exp a Name

xCopyArgsOfThunk a tRSrc tRDst xSrc xDst xIndex xLen
 = xApps a (XVar a $ fst utCopyArgsOfThunk)
        [ XType a tRSrc, XType a tRDst, xSrc, xDst, xIndex, xLen ]


utCopyArgsOfThunk :: (Bound Name, Type Name)
utCopyArgsOfThunk
 =      ( UName (NameVar "copyThunk")
        , tForalls [kRegion, kRegion]
           $ \[tR1, tR2] -> (tPtr tR1 tObj 
                                `tFun` tPtr tR2 tObj
                                `tFun` tNat `tFun` tNat 
                                `tFun` tPtr tR2 tObj))


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
           $ \[tR1, tR2] -> (tPtr tR1 tObj `tFun` tNat `tFun` tPtr tR2 tObj))


-- | Get the available arguments in a thunk.
xArgsOfThunk
        :: a -> Type Name
        -> Exp a Name -> Exp a Name

xArgsOfThunk a tR xThunk
 = xApps a (XVar a $ fst utArgsOfThunk)
        [ XType a tR, xThunk ]

utArgsOfThunk :: (Bound Name, Type Name)
utArgsOfThunk
 =      ( UName (NameVar "argsThunk")
        , tForall kRegion
           $ \tR -> (tPtr tR tObj `tFun` tNat))


-- | Set one of the argument pointers in a thunk.
xSetFieldOfThunk 
        :: a 
        -> Type Name    -- ^ Region containing thunk. 
        -> Type Name    -- ^ Region containigng new child.
        -> Exp a Name   -- ^ Thunk to set field of.
        -> Exp a Name   -- ^ Base offset.
        -> Exp a Name   -- ^ Index of field from base.
        -> Exp a Name   -- ^ New child value.
        -> Exp a Name

xSetFieldOfThunk a tR tC xObj xBase xIndex xVal
 = xApps a (XVar a $ fst utSetFieldOfThunk)
        [ XType a tR, XType a tC, xObj, xBase, xIndex, xVal]

utSetFieldOfThunk :: (Bound Name, Type Name)
utSetFieldOfThunk
 =      ( UName (NameVar "setThunk")
        , tForalls [kRegion, kRegion]
           $ \[tR1, tR2] 
           -> (tPtr tR1 tObj 
                        `tFun` tNat          `tFun` tNat 
                        `tFun` tPtr tR2 tObj `tFun` tVoid))


-- | Apply a thunk to some more arguments.
xApplyThunk
        :: a -> Int
        -> [Exp a Name] -> Exp a Name

xApplyThunk a arity xsArgs
 = xApps a (XVar a $ fst (utApplyThunk arity)) xsArgs

utApplyThunk :: Int -> (Bound Name, Type Name)
utApplyThunk arity
 = let  krThunk  = kRegion
        krsArg   = replicate arity kRegion
        krResult = kRegion
        ks       = [krThunk] ++ krsArg ++ [krResult]

        t       =  tForalls ks $ \rs
                -> let  (rThunk : rsMore) = rs
                        rsArg             = take arity rsMore
                        [rResult]         = drop arity rsMore
                        Just t' = tFunOfList 
                                $  [tPtr rThunk  tObj]
                                ++ [tPtr r       tObj | r <- rsArg]
                                ++ [tPtr rResult tObj]
                   in   t'

   in   ( UName (NameVar $ "apply" ++ show arity)
        , t )


-- | Run a thunk.
xRunThunk 
        :: a            -- ^ Annotation.
        -> Type Name    -- ^ Region containing thunk to run.
        -> Type Name    -- ^ Region containing result object.
        -> Exp a Name   -- ^ Expression of thunk to run.
        -> Exp a Name

xRunThunk a trThunk trResult xArg
 = xApps a (XVar a $ fst utRunThunk) 
        [XType a trThunk, XType a trResult, xArg]

utRunThunk :: (Bound Name, Type Name)
utRunThunk 
 =      ( UName (NameVar $ "runThunk")
        , tForalls [kRegion, kRegion] 
                $ \[tR1, tR2] -> tPtr tR1 tObj `tFun` tPtr tR2 tObj)


-- Boxed ------------------------------------------------------------------------------------------
-- | Allocate a Boxed object.
xAllocBoxed :: a -> Type Name -> Integer -> Exp a Name -> Exp a Name
xAllocBoxed a tR tag x2
 = xApps a (XVar a $ fst utAllocBoxed)
        [ XType a tR
        , XCon a (DaConPrim (NamePrimLit (PrimLitTag tag)) tTag)
        , x2]

utAllocBoxed :: (Bound Name, Type Name)
utAllocBoxed
 =      ( UName (NameVar "allocBoxed")
        , tForall kRegion $ \r -> (tTag `tFun` tNat `tFun` tPtr r tObj))


-- | Get a field of a Boxed object.
xGetFieldOfBoxed 
        :: a 
        -> Type Name    -- ^ Prime region var of object.
        -> Type Name    -- ^ Regino of result object.
        -> Exp a Name   -- ^ Object to update.
        -> Integer      -- ^ Field index.
        -> Exp a Name

xGetFieldOfBoxed a trPrime trField x2 offset
 = xApps a (XVar a $ fst utGetFieldOfBoxed) 
        [ XType a trPrime, XType a trField
        , x2
        , xNat a offset ]

utGetFieldOfBoxed :: (Bound Name, Type Name)
utGetFieldOfBoxed 
 =      ( UName (NameVar "getBoxed")
        , tForalls [kRegion, kRegion]
                $ \[r1, r2] 
                -> tPtr r1 tObj
                        `tFun` tNat 
                        `tFun` tPtr r2 tObj)


-- | Set a field in a Boxed Object.
xSetFieldOfBoxed 
        :: a 
        -> Type Name    -- ^ Prime region var of object.
        -> Type Name    -- ^ Region of field object.
        -> Exp a Name   -- ^ Object to update.
        -> Integer      -- ^ Field index.
        -> Exp a Name   -- ^ New field value.
        -> Exp a Name

xSetFieldOfBoxed a trPrime trField x2 offset val
 = xApps a (XVar a $ fst utSetFieldOfBoxed) 
        [ XType a trPrime, XType a trField
        , x2
        , xNat a offset
        , val]

utSetFieldOfBoxed :: (Bound Name, Type Name)
utSetFieldOfBoxed 
 =      ( UName (NameVar "setBoxed")
        , tForalls [kRegion, kRegion]
            $ \[r1, t2] -> tPtr r1 tObj `tFun` tNat `tFun` tPtr t2 tObj `tFun` tVoid)

-- Raw --------------------------------------------------------------------------------------------
-- | Allocate a Raw object.
xAllocRaw :: a -> Type Name -> Integer -> Exp a Name -> Exp a Name
xAllocRaw a tR tag x2
 = xApps a (XVar a $ fst utAllocRaw)
        [ XType a tR, xTag a tag, x2]

utAllocRaw :: (Bound Name, Type Name)
utAllocRaw
 =      ( UName (NameVar "allocRaw")
        , tForall kRegion $ \r -> (tTag `tFun` tNat `tFun` tPtr r tObj))


-- | Get the payload of a Raw object.
xPayloadOfRaw :: a -> Type Name -> Exp a Name -> Exp a Name
xPayloadOfRaw a tR x2 
 = xApps a (XVar a $ fst utPayloadOfRaw) 
        [XType a tR, x2]
 
utPayloadOfRaw :: (Bound Name, Type Name)
utPayloadOfRaw
 =      ( UName (NameVar "payloadRaw")
        , tForall kRegion $ \r -> (tFun (tPtr r tObj) (tPtr r (tWord 8))))


-- Small ------------------------------------------------------------------------------------------
-- | Allocate a Small object.
xAllocSmall :: a -> Type Name -> Integer -> Exp a Name -> Exp a Name
xAllocSmall a tR tag x2
 = xApps a (XVar a $ fst utAllocSmall)
        [ XType a tR, xTag a tag, x2]

utAllocSmall :: (Bound Name, Type Name)
utAllocSmall
 =      ( UName (NameVar "allocSmall")
        , tForall kRegion $ \r -> (tTag `tFun` tNat `tFun` tPtr r tObj))


-- | Get the payload of a Small object.
xPayloadOfSmall :: a -> Type Name -> Exp a Name -> Exp a Name
xPayloadOfSmall a tR x2 
 = xApps a (XVar a $ fst utPayloadOfSmall) 
        [XType a tR, x2]
 
utPayloadOfSmall :: (Bound Name, Type Name)
utPayloadOfSmall
 =      ( UName (NameVar "payloadSmall")
        , tForall kRegion $ \r -> (tFun (tPtr r tObj) (tPtr r (tWord 8))))


-- Error ------------------------------------------------------------------------------------------
-- | Get the payload of a Small object.
xErrorDefault :: a -> Exp a Name -> Exp a Name -> Exp a Name
xErrorDefault a xStr xLine
 = xApps a (XVar a $ fst utErrorDefault) 
           [xStr, xLine]
 
utErrorDefault :: (Bound Name, Type Name)
utErrorDefault
 =      ( UName (NameVar "primErrorDefault")
        , tTextLit `tFun` tNat `tFun` tPtr rTop tObj)

