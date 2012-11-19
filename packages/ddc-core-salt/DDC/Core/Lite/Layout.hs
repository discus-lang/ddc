
-- | Layout of algebraic data.
module DDC.Core.Lite.Layout
        ( -- * Heap Objects
          HeapObject(..)
        , heapObjectOfDataCtor

          -- * Fields
        , payloadSizeOfDataCtor
        , fieldOffsetsOfDataCtor)
where
import DDC.Core.Lite.Name
import DDC.Core.Lite.Env
import DDC.Core.Salt.Platform
import DDC.Type.DataDef
import DDC.Type.Exp
import Control.Monad
import Data.Maybe
import qualified DDC.Core.Salt.Name     as A


-- HeapObject -----------------------------------------------------------------
-- | Enumerates the heap object formats that can be used to store
--   algebraic data.
--
--   The layout of these is defined in the @ObjectNN.dce@ file of the runtime 
--   system, where @NN@ is the word size of the machine.
data HeapObject
        = HeapObjectBoxed
        | HeapObjectMixed
        | HeapObjectRaw
        | HeapObjectRawSmall
        deriving (Eq, Show)


-- | Decide which heap object to use to represent a data constructor.
heapObjectOfDataCtor :: Platform -> DataCtor Name -> Maybe HeapObject
heapObjectOfDataCtor pp ctor

        -- If all the fields are boxed objects then used a Boxed heap object, 
        -- as these just contain pointer fields.
        | tsFields              <- dataCtorFieldTypes ctor
        , Just unboxeds         <- sequence $ map isUnboxedType tsFields
        , all not unboxeds
        = Just HeapObjectBoxed

        -- All of the fixed size primitive types will fit in a RawSmall object.
        --   Each field needs to be non-abstract, and have a real width.
        | [t@(TCon tc)]            <- dataCtorFieldTypes ctor
        , Just True                <- isUnboxedType t
        , TyConBound (UPrim n _) _ <- tc
        , NamePrimTyCon ptc        <- n
        , isJust $ A.primTyConWidth pp ptc
        = Just HeapObjectRawSmall

        | otherwise
        = Nothing


-- Field Layout ---------------------------------------------------------------
-- | Get the size of the payload for this data constructor.
--   The payload holds all the fields, but does not include
--   header information such as the constructor tag.
--
--   This doesn't add any padding for misaligned fields.
payloadSizeOfDataCtor :: Platform -> DataCtor Name -> Maybe Integer
payloadSizeOfDataCtor platform ctor
        = liftM sum
        $ sequence
        $ map (fieldSizeOfType platform)
        $ dataCtorFieldTypes ctor


-- | Given a constructor definition,
--   get the offset of each field in the payload of a heap object.
--
--   We don't know the absolute offset from the beginning of the heap 
--   object, because the size of the header is only known by the runtime 
--   system.
--
--   This doesn't add any padding for misaligned fields.
fieldOffsetsOfDataCtor :: Platform -> DataCtor Name -> Maybe [Integer]
fieldOffsetsOfDataCtor platform ctor
        = liftM (init . scanl (+) 0)
        $ sequence 
        $ map (fieldSizeOfType platform) 
        $ dataCtorFieldTypes ctor


-- | Get the raw size of a field of this type, without padding.
fieldSizeOfType    :: Platform -> Type Name -> Maybe Integer
fieldSizeOfType platform tt
 = case tt of
        TVar{}          -> Just $ platformAddrBytes platform

        TCon tc
         -> case tc of
                TyConBound (UPrim n _) _ -> fieldSizeOfPrim platform n
                TyConBound _ _           -> Just $ platformAddrBytes platform
                _                        -> Nothing

        -- We're not supporting polymorphic fields yet.
        TForall{}       -> Nothing

        -- Assume any non-unboxed thing is represented by a pointer.
        TApp{}          -> Just $ platformAddrBytes platform

        -- We shouldn't find any TSums, because field types always have
        -- kind data.
        TSum{}          -> Nothing


fieldSizeOfPrim :: Platform -> Name -> Maybe Integer
fieldSizeOfPrim platform nn
 = case nn of
        NameDataTyCon{}         -> Just $ platformAddrBytes platform
        NamePrimTyCon tc        -> fieldSizeOfPrimTyCon platform tc
        _                       -> Nothing

fieldSizeOfPrimTyCon :: Platform -> PrimTyCon -> Maybe Integer
fieldSizeOfPrimTyCon platform tc
 = case tc of
        -- It might make sense to represent these as zero bytes, 
        -- but I can't think of reason to have them in data type definitions.
        PrimTyConVoid        -> Nothing

        -- Pointer tycon shouldn't appear by itself.
        PrimTyConPtr         -> Nothing

        PrimTyConAddr        -> Just $ platformAddrBytes platform
        PrimTyConNat         -> Just $ platformNatBytes  platform
        PrimTyConInt         -> Just $ platformNatBytes  platform
        PrimTyConTag         -> Just $ platformTagBytes  platform
        PrimTyConBool        -> Just $ 1

        PrimTyConWord bits
         | bits `mod` 8 == 0 -> Just $ fromIntegral $ bits `div` 8
         | otherwise         -> Nothing

        PrimTyConFloat bits
         | bits `mod` 8 == 0 -> Just $ fromIntegral $ bits `div` 8
         | otherwise         -> Nothing

        -- Strings shouldn't appear as raw fields, only pointers to them.
        PrimTyConString      -> Nothing
