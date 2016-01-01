
-- | Punned data type and constructor definitions for boxed numeric objects.
--
--   Boxed numeric objects are treated abstractly by the source language, and
--   aren't really algebraic data, but we define them as such so that we can
--   re-use the to-salt conversion code for algebraic data.
--
--   Each primitive numeric type like (Nat#) induces a data type and data
--   constructor of the same name.
--
--   The data constructor has a single unboxed field (U# Nat#) and produces
--   a boxed result type (B# Nat#). Note that the name of the data type (Nat#)
--   is different from the result type (B# Nat#), which is unlike real algebraic
--   data types.
--
module DDC.Core.Tetra.Convert.Boxing
        ( isSomeRepType
        , isBoxedRepType
        , isUnboxedRepType
        , isNumericType
        , isTextLitType
        , makeBoxedPrimDataType
        , makeBoxedPrimDataCtor)
where
import DDC.Core.Tetra.Prim
import DDC.Core.Tetra.Compounds
import DDC.Type.DataDef
import DDC.Type.Exp


-- Predicates -----------------------------------------------------------------
-- | Check if this is a representable type.
--   This is the union of `isBoxedRepType` and `isUnboxedRepType`.
isSomeRepType :: Type Name -> Bool
isSomeRepType tt
        = isBoxedRepType tt || isUnboxedRepType tt


-- | Check if some representation type is boxed.
--   The type must have kind Data, otherwise bogus result.
--
--   A "representation type" is the sort of type we get after applying the
--   Boxing transform, which works out how to represent everything.
--
--   The boxed representation types are:
--      1) 'a -> b'     -- the function type
--      1) 'a'          -- polymorphic types.
--      2) 'forall ...' -- abstract types.
--      3) 'Unit'       -- the unit data type.
--      5) User defined data types.
--
isBoxedRepType :: Type Name -> Bool
isBoxedRepType tt
        | Just _        <- takeTFun tt
        = True

        | TVar{}        <- tt   = True
        | TForall{}     <- tt   = True

        -- Unit data type.
        | Just (TyConSpec TcConUnit, _)         <- takeTyConApps tt
        = True

        -- User defined data types.
        | Just (TyConBound (UName _) _, _)      <- takeTyConApps tt
        = True

        -- Boxed numeric types
        | isNumericType tt
        = True

        | otherwise
        = False


-- | Check if some representation type is unboxed.
--   The type must have kind Data, otherwise bogus result.
--
--   A "representation type" is the sort of type we get after applying the
--   Boxing transform, which works out how to represent everything.
--
--   The unboxed representation are are:
--      1) 'U# T'     -- unboxed numeric types, where T is a boxable type.
--
isUnboxedRepType :: Type Name -> Bool
isUnboxedRepType tt
        | Just ( NameTyConTetra TyConTetraU
               , [ti])                  <- takePrimTyConApps tt
        , isNumericType ti || isTextLitType ti
        = True

        | otherwise
        = False


-- | Check if some type is a numeric type.
--   These are:
--      Nat#, Int#, WordN# and so on
--     TODO: also now TextLit#
--
isNumericType :: Type Name -> Bool
isNumericType tt
        | Just (NamePrimTyCon n, [])   <- takePrimTyConApps tt
        = case n of
                PrimTyConBool           -> True
                PrimTyConNat            -> True
                PrimTyConInt            -> True
                PrimTyConWord  _        -> True
                PrimTyConFloat _        -> True
                PrimTyConTextLit        -> True
                _                       -> False

        | otherwise                     = False


-- | Check if this is the string type.
--   TODO: ditch this.
isTextLitType :: Type Name -> Bool
isTextLitType tt
        | Just (NamePrimTyCon n, [])    <- takePrimTyConApps tt
        = case n of
                PrimTyConTextLit        -> True
                _                       -> False

        | otherwise                     = False


-- Punned Defs ----------------------------------------------------------------
-- | Generic data type definition for a primitive numeric type.
makeBoxedPrimDataType :: Type Name -> Maybe (DataType Name)
makeBoxedPrimDataType tt
        | Just (n@NamePrimTyCon{}, []) <- takePrimTyConApps tt
        = Just $ DataType 
        { dataTypeName          = n
        , dataTypeParams        = []
        , dataTypeMode          = DataModeLarge
        , dataTypeIsAlgebraic   = False }

        | otherwise
        = Nothing


-- | Generic data constructor definition for a primtive numeric type.
makeBoxedPrimDataCtor :: Type Name -> Maybe (DataCtor Name)
makeBoxedPrimDataCtor tt
        | Just (n@NamePrimTyCon{}, []) <- takePrimTyConApps tt
        = Just $ DataCtor
        { dataCtorName          = n
        , dataCtorTag           = 0
        , dataCtorFieldTypes    = [tUnboxed tt]
        , dataCtorResultType    = tt
        , dataCtorTypeName      = n
        , dataCtorTypeParams    = [] }

        | otherwise
        = Nothing

