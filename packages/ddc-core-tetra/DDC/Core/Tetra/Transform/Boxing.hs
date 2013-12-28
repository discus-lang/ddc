
module DDC.Core.Tetra.Transform.Boxing
        (boxingModule)
where
import DDC.Core.Tetra.Compounds
import DDC.Core.Tetra.Prim
import DDC.Core.Transform.Boxing
import DDC.Core.Module
import DDC.Core.Exp


-- | Manage boxing of numeric values in a module.
boxingModule :: Module a Name -> Module a Name
boxingModule mm
        = boxing config mm


-- | Tetra-specific configuration for boxing transform.
config :: Config a Name
config  = Config
        { configRepOfType               = repOfType
        , configTakeTypeBoxed           = takeTypeBoxed
        , configTakeTypeUnboxed         = takeTypeUnboxed
        , configNameIsLiteral           = isNameLit
        , configNameIsUnboxedOp         = isNameOfUnboxedOp 
        , configTypeOfPrimOpName        = takeTypeOfPrimOpName 
        , configBoxLiteral              = boxLiteral
        , configBoxExp                  = boxExp
        , configUnboxExp                = unboxExp }


-- | Get the representation of some type of kind Data.
--
--   If the type does not have kind Data then you'll get a bogus result.
repOfType :: Type Name -> Rep
repOfType tt
        | Just (NamePrimTyCon n, _)     <- takePrimTyConApps tt
        = case n of
                PrimTyConVoid           -> RepNone
                PrimTyConBool           -> RepUnboxed
                PrimTyConNat            -> RepUnboxed
                PrimTyConInt            -> RepUnboxed
                PrimTyConWord{}         -> RepUnboxed
                PrimTyConFloat{}        -> RepUnboxed
                PrimTyConVec{}          -> RepNone
                PrimTyConAddr{}         -> RepUnboxed
                PrimTyConPtr{}          -> RepNone
                PrimTyConTag{}          -> RepUnboxed
                PrimTyConString{}       -> RepUnboxed

        -- These are all higher-kinded type constructors,
        -- with don't have a value-level representation.
        | Just (NameTyConTetra n, _)    <- takePrimTyConApps tt
        = case n of
                TyConTetraRef{}         -> RepNone
                TyConTetraTuple{}       -> RepNone
                TyConTetraB{}           -> RepNone
                TyConTetraU{}           -> RepNone

        | otherwise
        = RepBoxed


-- | Get the boxed version of some type of kind Data.
takeTypeBoxed :: Type Name -> Maybe (Type Name)
takeTypeBoxed tt
        | Just (NamePrimTyCon tc, [])   <- takePrimTyConApps tt
        = case tc of
                PrimTyConBool           -> Just $ tBoxed tBool
                PrimTyConNat            -> Just $ tBoxed tNat
                PrimTyConInt            -> Just $ tBoxed tInt
                PrimTyConWord  bits     -> Just $ tBoxed (tWord  bits)
                _                       -> Nothing

        | otherwise     = Nothing


-- | Get the unboxed version of some type of kind Data.
takeTypeUnboxed :: Type Name -> Maybe (Type Name)
takeTypeUnboxed tt
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


-- | Convert a literal name to a boxed representation.
boxLiteral :: a -> Name -> Maybe (Exp a Name)
boxLiteral a nLit
        | Just tLit    <- takeTypeOfLitName nLit
        , Just tResult <- takeTypeBoxed tLit
        = Just 
        $ xCastConvert a
                tResult tLit 
                (XVar a (UPrim nLit tLit))

        | otherwise
        = Nothing


-- | Box an expression of the given type.
boxExp   :: a -> Exp a Name -> Type Name -> Maybe (Exp a Name)
boxExp a xx tt
        | Just tBx      <- takeTypeBoxed tt
        , Just tUx      <- takeTypeUnboxed tt
        = Just $ xCastConvert a tBx tUx xx

        | otherwise     = Nothing


-- | Unbox an expression of the given type.
unboxExp :: a -> Exp a Name -> Type Name -> Maybe (Exp a Name)
unboxExp a xx tt
        | Just tBx      <- takeTypeBoxed tt
        , Just tUx      <- takeTypeUnboxed tt
        = Just $ xCastConvert a tUx tBx xx

        | otherwise     = Nothing

