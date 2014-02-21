
module DDC.Core.Tetra.Transform.Boxing
        (boxingModule)
where
import DDC.Core.Tetra.Compounds
import DDC.Core.Tetra.Prim
import DDC.Core.Transform.Boxing
import DDC.Core.Module
import DDC.Core.Exp


-- | Manage boxing of numeric values in a module.
boxingModule :: Show a => Module a Name -> Module a Name
boxingModule mm
        = boxing config mm


-- | Tetra-specific configuration for boxing transform.
config :: Config a Name
config  = Config
        { configIsValueIndexType        = isValueIndexType
        , configIsBoxedType             = isBoxedType
        , configIsUnboxedType           = isUnboxedType
        , configBoxedOfIndexType        = boxedOfIndexType
        , configUnboxedOfIndexType      = unboxedOfIndexType
        , configIndexTypeOfBoxed        = indexTypeOfBoxed
        , configIndexTypeOfUnboxed      = indexTypeOfUnboxed
        , configNameIsUnboxedOp         = isNameOfUnboxedOp 
        , configValueTypeOfLitName      = takeTypeOfLitName
        , configValueTypeOfPrimOpName   = takeTypeOfPrimOpName
        , configValueTypeOfForeignName  = const Nothing
        , configBoxedOfValue            = boxedOfValue
        , configValueOfBoxed            = valueOfBoxed
        , configBoxedOfUnboxed          = boxedOfUnboxed
        , configUnboxedOfBoxed          = unboxedOfBoxed }


-- | Check whether a value of this type needs boxing to make the 
--   program representational.
isValueIndexType :: Type Name -> Bool
isValueIndexType tt
        -- These types are listed out in full so anyone who adds more 
        -- constructors to the PrimTyCon type is forced to say whether
        -- those types refer to unboxed values or not.
        --
        | Just (NamePrimTyCon n, _)     <- takePrimTyConApps tt
        = case n of
                -- There should never be any value of type Void# being passed
                -- around, but say they don't need boxing anyway so we don't 
                -- complicate an already broken program.
                PrimTyConVoid           -> False

                PrimTyConBool           -> True
                PrimTyConNat            -> True
                PrimTyConInt            -> True
                PrimTyConWord{}         -> True
                PrimTyConFloat{}        -> True
                PrimTyConVec{}          -> True
                PrimTyConAddr{}         -> True
                PrimTyConPtr{}          -> True
                PrimTyConTag{}          -> True
                PrimTyConString{}       -> True

        -- These are all higher-kinded type constructors,
        -- with don't have a value-level representation.
        | Just (NameTyConTetra n, _)    <- takePrimTyConApps tt
        = case n of
                TyConTetraRef{}         -> False
                TyConTetraTuple{}       -> False
                TyConTetraB{}           -> False
                TyConTetraU{}           -> False

        | otherwise
        = False


-- | Check whether this is a boxed representation type.
isBoxedType :: Type Name -> Bool
isBoxedType tt
        | Just (n, _)   <- takePrimTyConApps tt
        , NameTyConTetra TyConTetraB    <- n
        = True

        | otherwise = False


-- | Check whether this is a boxed representation type.
isUnboxedType :: Type Name -> Bool
isUnboxedType tt
        | Just (n, _)   <- takePrimTyConApps tt
        , NameTyConTetra TyConTetraU    <- n
        = True

        | otherwise = False


-- | Take the index type from a boxed type, if it is one.
indexTypeOfBoxed :: Type Name -> Maybe (Type Name)
indexTypeOfBoxed tt
        | Just (n, [t]) <- takePrimTyConApps tt
        , NameTyConTetra TyConTetraB    <- n
        = Just t

        | otherwise
        = Nothing


-- | Take the index type from an unboxed type, if it is one.
indexTypeOfUnboxed :: Type Name -> Maybe (Type Name)
indexTypeOfUnboxed tt
        | Just (n, [t]) <- takePrimTyConApps tt
        , NameTyConTetra TyConTetraU    <- n
        = Just t

        | otherwise
        = Nothing


-- | Get the boxed version of some type of kind Data.
boxedOfIndexType :: Type Name -> Maybe (Type Name)
boxedOfIndexType tt
        | Just (NamePrimTyCon tc, [])   <- takePrimTyConApps tt
        = case tc of
                PrimTyConBool           -> Just $ tBoxed tBool
                PrimTyConNat            -> Just $ tBoxed tNat
                PrimTyConInt            -> Just $ tBoxed tInt
                PrimTyConWord  bits     -> Just $ tBoxed (tWord  bits)
                _                       -> Nothing

        | otherwise     = Nothing


-- | Get the unboxed version of some type of kind Data.
unboxedOfIndexType :: Type Name -> Maybe (Type Name)
unboxedOfIndexType tt
        | Just (NamePrimTyCon tc, [])   <- takePrimTyConApps tt
        = case tc of
                PrimTyConBool           -> Just $ tUnboxed tBool
                PrimTyConNat            -> Just $ tUnboxed tNat
                PrimTyConInt            -> Just $ tUnboxed tInt
                PrimTyConWord  bits     -> Just $ tUnboxed (tWord  bits)
                _                       -> Nothing

        | otherwise     = Nothing


-- | Check if the primitive operator with this name takes unboxed values
--   directly.
isNameOfUnboxedOp :: Name -> Bool
isNameOfUnboxedOp nn
 = case nn of
        NamePrimArith{} -> True
        NamePrimCast{}  -> True
        _               -> False


-- | Wrap a pure value into its boxed representation.
boxedOfValue :: a -> Exp a Name -> Type Name -> Maybe (Exp a Name)
boxedOfValue a xx tt
        | Just tBx      <- boxedOfIndexType tt
        = Just $ xCastConvert a tt tBx xx

        | otherwise     = Nothing


-- | Unwrap a boxed value.
valueOfBoxed :: a -> Exp a Name -> Type Name -> Maybe (Exp a Name)
valueOfBoxed a xx tt
        | Just tBx      <- boxedOfIndexType tt
        = Just $ xCastConvert a tBx tt xx

        | otherwise     = Nothing


-- | Box an expression of the given type.
boxedOfUnboxed :: a -> Exp a Name -> Type Name -> Maybe (Exp a Name)
boxedOfUnboxed a xx tt
        | Just tBx      <- boxedOfIndexType tt
        , Just tUx      <- unboxedOfIndexType tt
        = Just $ xCastConvert a tUx tBx xx

        | otherwise     = Nothing


-- | Unbox an expression of the given type.
unboxedOfBoxed :: a -> Exp a Name -> Type Name -> Maybe (Exp a Name)
unboxedOfBoxed a xx tt
        | Just tBx      <- boxedOfIndexType tt
        , Just tUx      <- unboxedOfIndexType tt
        = Just $ xCastConvert a tBx tUx xx

        | otherwise     = Nothing

