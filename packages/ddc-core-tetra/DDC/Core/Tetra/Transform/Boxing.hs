
module DDC.Core.Tetra.Transform.Boxing
        (boxingModule)
where
import DDC.Core.Tetra.Compounds
import DDC.Core.Tetra.Prim
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Transform.Boxing           (Rep(..), Config(..))
import qualified DDC.Core.Transform.Boxing as Boxing


-- | Manage boxing of numeric values in a module.
boxingModule :: Show a => Module a Name -> Module a Name
boxingModule mm 
 = let
        tsForeignSea    
         = [ (n, t) | (n, ImportValueSea _ t) <- moduleImportValues mm]

   in   Boxing.boxingModule (config tsForeignSea) mm


-- | Tetra-specific configuration for boxing transform.
config :: [(Name, Type Name)] -> Config a Name
config ntsForeignSea  
        = Config
        { configRepOfType               = repOfType
        , configConvertRepType          = convertRepType
        , configConvertRepExp           = convertRepExp
        , configValueTypeOfLitName      = takeTypeOfLitName
        , configValueTypeOfPrimOpName   = takeTypeOfPrimOpName
        , configValueTypeOfForeignName  = \n -> lookup n ntsForeignSea
        , configUnboxPrimOpName         = unboxPrimOpName
        , configUnboxLitName            = unboxLitName }


-- | Get the representation of a given type.
repOfType :: Type Name -> Maybe Rep
repOfType tt
        -- These types are listed out in full so anyone who adds more
        -- constructors to the PrimTyCon type is forced to specify what
        -- the representation is.
        | Just (NamePrimTyCon n, _)     <- takePrimTyConApps tt
        = case n of
                PrimTyConVoid           -> Just RepNone

                PrimTyConBool           -> Just RepBoxed
                PrimTyConNat            -> Just RepBoxed
                PrimTyConInt            -> Just RepBoxed
                PrimTyConSize           -> Just RepBoxed
                PrimTyConWord{}         -> Just RepBoxed
                PrimTyConFloat{}        -> Just RepBoxed
                PrimTyConVec{}          -> Just RepBoxed
                PrimTyConAddr{}         -> Just RepBoxed
                PrimTyConPtr{}          -> Just RepBoxed
                PrimTyConTextLit{}      -> Just RepBoxed
                PrimTyConTag{}          -> Just RepBoxed

        -- Explicitly unboxed things.
        | Just (n, _)   <- takePrimTyConApps tt
        , NameTyConTetra TyConTetraU    <- n
        = Just RepUnboxed

        | Just (NameTyConTetra n, _)    <- takePrimTyConApps tt
        = case n of
                -- These are all higher-kinded type constructors,
                -- which don't have any associated values.
                TyConTetraTuple{}       -> Just RepNone
                TyConTetraVector{}      -> Just RepNone
                TyConTetraU{}           -> Just RepNone
                TyConTetraF{}           -> Just RepNone
                TyConTetraC{}           -> Just RepNone


        | otherwise
        = Nothing


-- | Get the type for a different representation of the given one.
convertRepType :: Rep -> Type Name -> Maybe (Type Name)
convertRepType RepBoxed tt
        -- Produce the value type from an unboxed one.
        | Just (n, [t]) <- takePrimTyConApps tt
        , NameTyConTetra TyConTetraU    <- n
        = Just t

convertRepType RepUnboxed tt
        | Just (NamePrimTyCon tc, [])   <- takePrimTyConApps tt
        = case tc of
                PrimTyConBool           -> Just $ tUnboxed tBool
                PrimTyConNat            -> Just $ tUnboxed tNat
                PrimTyConInt            -> Just $ tUnboxed tInt
                PrimTyConSize           -> Just $ tUnboxed tSize
                PrimTyConWord  bits     -> Just $ tUnboxed (tWord  bits)
                PrimTyConFloat bits     -> Just $ tUnboxed (tFloat bits) 
                PrimTyConTextLit        -> Just $ tUnboxed tTextLit
                _                       -> Nothing

        | Just (NameTyConTetra tc, [])   <- takePrimTyConApps tt
        = case tc of
                _                       -> Nothing

convertRepType _ _
        = Nothing


-- | Convert an expression from one representation to another.
convertRepExp :: Rep -> a -> Type Name -> Exp a Name -> Maybe (Exp a Name)
convertRepExp rep a tSource xx
        | Just tResult  <- convertRepType rep tSource
        = Just $ xCastConvert a tSource tResult xx

        | otherwise
        = Nothing


{-
-- | Check if the primitive operator with this name takes unboxed values
--   directly.
isNameOfUnboxedOp :: Name -> Bool
isNameOfUnboxedOp nn
 = case nn of
        NamePrimArith{} -> True
        NamePrimCast{}  -> True
        NameOpVector{}  -> True
        _               -> False
-}

-- | Convert a primitive operator name to the unboxed version.
unboxPrimOpName :: Name -> Maybe Name
unboxPrimOpName n
 = case n of
        -- The types of arithmetic operators are already polytypic,
        -- and can be instantiated at either value types or unboxed types.
        NamePrimArith op        -> Just $ NamePrimArith  op

        -- The types of vector operators have different value type and unboxed versions.
        NameOpVector  op False  -> Just $ NameOpVector op True

        _                       -> Nothing


-- | If this is the name of an literal, then produce the unboxed version.
unboxLitName :: Name -> Maybe Name
unboxLitName n
        | isNameLit n && not (isNameLitUnboxed n)
        = Just $ NameLitUnboxed n

        | otherwise
        = Nothing
