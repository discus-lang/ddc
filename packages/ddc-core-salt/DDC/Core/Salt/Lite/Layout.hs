
-- | Layout of algebraic data.
module DDC.Core.Salt.Lite.Layout
        ( -- * Heap Objects
          HeapObject(..)
        , heapObjectOfDataCtor

          -- * Field Offsets
        , fieldOffsetsOfDataCtor

          -- * Field types
        , isUnboxedType
        , isFixedSizePrimTyCon)
where
import DDC.Core.Salt.Lite.Name
import DDC.Core.Salt.Base.Name
import DDC.Core.Salt.Platform
import DDC.Type.DataDef
import DDC.Type.Exp
import Control.Monad

-- HeapObject -----------------------------------------------------------------
-- | Enumerates the heap object formats that can be used to store
--   algebraic data.
--
--   The layout of these is defined in the ObjectNN.dce file of the runtime 
--   system, where NN is the word size of the machine.
data HeapObject
        = HeapObjectBoxed
        | HeapObjectMixed
        | HeapObjectRaw
        | HeapObjectRawSmall
        deriving (Eq, Show)


-- | Decide which heap object to use to represent a data contructor.
heapObjectOfDataCtor :: DataCtor Name -> Maybe HeapObject
heapObjectOfDataCtor ctor

        -- If all the fields are boxed objects then used a Boxed heap object, 
        -- as these just contain pointer fields.
        | tsFields              <- dataCtorFieldTypes ctor
        , Just unboxeds         <- sequence $ map isUnboxedType tsFields
        , all not unboxeds
        = Just HeapObjectBoxed

        -- All of the fixed size primitive types will fit in a RawSmall object.
        | [t@(TCon tc)]          <- dataCtorFieldTypes ctor
        , Just True              <- isUnboxedType t
        , TyConBound (UPrim n _) <- tc
        , NamePrimTyCon ptc      <- n
        , Just True              <- isFixedSizePrimTyCon ptc
        = Just HeapObjectRawSmall

        | otherwise
        = Nothing


-- Field Layout ---------------------------------------------------------------
-- | Given a constructor definition,
--   get the offset of each field in the payload of a heap object.
--
--   We don't know the offset from the start of the overall object, 
--   because the size of the header is only known by the runtime system.
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
                TyConBound (UPrim n _)  -> fieldSizeOfPrim platform n
                TyConBound _            -> Just $ platformAddrBytes platform
                _                       -> Nothing

        -- We're not supporting polymorphic fields yet.
        TForall{}       -> Nothing

        -- TODO: pass through applications of pointers.
        TApp{}          -> Just $ platformAddrBytes platform

        -- We shouldn't find any TSums, because field types always have
        -- kind data.
        TSum{}          -> Nothing


fieldSizeOfPrim :: Platform -> Name -> Maybe Integer
fieldSizeOfPrim platform nn
 = case nn of
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
        PrimTyConTag         -> Just $ platformTagBytes  platform
        PrimTyConBool        -> Just $ 1

        -- Strings shouldn't appear as raw fields, only pointers to them.
        PrimTyConString      -> Nothing

        PrimTyConWord bits
         | bits `mod` 8 == 0 -> Just $ fromIntegral $ bits `div` 8
         | otherwise         -> Nothing

        PrimTyConInt bits
         | bits `mod` 8 == 0 -> Just $ fromIntegral $ bits `div` 8
         | otherwise         -> Nothing

        PrimTyConFloat bits
         | bits `mod` 8 == 0 -> Just $ fromIntegral $ bits `div` 8
         | otherwise         -> Nothing


-- Unboxed --------------------------------------------------------------------
-- | Check whether a type is represents an unboxed object.
--   Returns `Nothing` for sums, as this cannot be a data type.
isUnboxedType :: Type Name -> Maybe Bool
isUnboxedType tt
 = case tt of
        TVar{}          -> Just False
        TCon tc
         | TyConBound (UPrim n _) <- tc
         , NamePrimTyCon _        <- n    -> Just True
         | otherwise                      -> Just False

        TForall{}       -> Just False

        -- TODO: handle application of pointer constructor.
        TApp{}          -> Just False

        TSum{}          -> Nothing


-- | Check whether a primitive type constructor represents a value with a fixed
--   size. Types WordN# and IntN# have a fixed size, but String# does not.
--
--   Returns `Nothing` for void and (unapplied) pointers.
isFixedSizePrimTyCon :: PrimTyCon -> Maybe Bool
isFixedSizePrimTyCon tc
 = case tc of
        PrimTyConVoid    -> Nothing
        PrimTyConPtr     -> Nothing
        PrimTyConAddr    -> Just True
        PrimTyConNat     -> Just True
        PrimTyConTag     -> Just True
        PrimTyConBool    -> Just True
        PrimTyConString  -> Just False
        PrimTyConWord{}  -> Just True
        PrimTyConInt{}   -> Just True
        PrimTyConFloat{} -> Just True

