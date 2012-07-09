
-- | Convert Sea types to LLVM types.
module DDC.Core.Llvm.Convert.Type
        ( -- * Type conversion.
          convTypeM
        , convType
        , llvmParameterOfType
        , importedFunctionDeclOfType

          -- * Builtin Types
        , tObj, sObj,  aObj
        , tPtr, tAddr, tNat, tInt, tTag

          -- * Type Constructors
        , convTyCon

          -- * Predicates
        , isVoidT
        , isSignedT
        , isUnsignedT
        , isIntegralT
        , isFloatingT)
where
import DDC.Llvm.Attr
import DDC.Llvm.Type
import DDC.Core.Llvm.LlvmM
import DDC.Core.Salt.Sanitize
import DDC.Core.Salt.Platform
import DDC.Type.Compounds
import Control.Monad.State.Strict
import DDC.Core.Salt                    as A
import qualified DDC.Core.Module        as C
import qualified DDC.Core.Exp           as C


-- Type -----------------------------------------------------------------------
convTypeM :: C.Type Name -> LlvmM Type
convTypeM tt
 = do   platform <- gets llvmStatePlatform
        return   $ convType platform tt


-- | Convert a Sea type to an LlvmType.
convType :: Platform -> C.Type Name -> Type
convType platform tt
 = case tt of
        -- A primitive type.
        C.TCon tc
          -> convTyCon platform tc

        -- A pointer to a primitive type.
        C.TApp (C.TCon (C.TyConBound (C.UPrim (NamePrimTyCon PrimTyConPtr) _))) t2
         -> TPointer (convType platform t2)

        -- Function types become pointers to functions.
        C.TApp{}
         |  (tsArgs, tResult) <- takeTFunArgResult tt
         -> TPointer $ TFunction 
         $  FunctionDecl
             { declName          = "dummy.function.name"
             , declLinkage       = Internal
             , declCallConv      = CC_Ccc
             , declReturnType    = convType platform tResult
             , declParamListType = FixedArgs
             , declParams        = map (llvmParameterOfType platform) tsArgs
             , declAlign         = AlignBytes (platformAlignBytes platform) }


        _ -> die ("Invalid Type " ++ show tt)


-- | Convert an imported function type to a LLVM declaration.
importedFunctionDeclOfType 
        :: Platform 
        -> Linkage 
        -> C.QualName Name 
        -> C.Type Name 
        -> Maybe FunctionDecl

importedFunctionDeclOfType platform linkage (C.QualName _ (NameVar n)) tt   -- TODO: handle module qualifiers
 = let  (tsArgs@(_ : _), tResult) = takeTFunArgResult tt
   in   Just $ FunctionDecl
             { declName           = sanitizeName  n
             , declLinkage        = linkage
             , declCallConv       = CC_Ccc
             , declReturnType     = convType platform tResult
             , declParamListType  = FixedArgs
             , declParams         = map (llvmParameterOfType platform) tsArgs
             , declAlign          = AlignBytes (platformAlignBytes platform) }

importedFunctionDeclOfType _ _ _ _
        = Nothing


-- | Convert a parameter type to a LlvmParameter decl.
llvmParameterOfType :: Platform -> C.Type Name -> Param
llvmParameterOfType platform tt
        = Param
        { paramType     = convType platform tt
        , paramAttrs    = [] }


-- TyCon ----------------------------------------------------------------------
-- | Convert a Sea TyCon to a LlvmType.
convTyCon :: Platform -> C.TyCon Name -> Type
convTyCon platform tycon
 = case tycon of
        C.TyConBound (C.UPrim NameObjTyCon _)
         -> tObj platform

        C.TyConBound (C.UPrim (NamePrimTyCon tc) _)
         -> case tc of
                PrimTyConVoid           -> TVoid
                PrimTyConBool           -> TInt 1
                PrimTyConNat            -> TInt (8 * platformAddrBytes platform)
                PrimTyConInt            -> TInt (8 * platformAddrBytes platform)
                PrimTyConWord bits      -> TInt (fromIntegral bits)
                PrimTyConTag            -> TInt (8 * platformTagBytes  platform)
                PrimTyConAddr           -> TInt (8 * platformAddrBytes platform)
                PrimTyConString         -> TPointer (TInt 8)

                PrimTyConFloat bits
                 -> case bits of
                        32              -> TFloat
                        64              -> TDouble
                        80              -> TFloat80
                        128             -> TFloat128
                        _               -> die "Invalid width for float type constructor."

                _                       -> die "Invalid primitive type constructor."

        _ -> die "Invalid type constructor."


-- | Type of Heap objects.
sObj, tObj :: Platform -> Type
sObj platform   = TStruct [TInt (8 * platformObjBytes platform)]
tObj platform   = TAlias (aObj platform)

aObj :: Platform -> TypeAlias
aObj platform   = TypeAlias "s.Obj" (sObj platform)


-- | Alias for pointer type.
tPtr :: Type -> Type
tPtr t = TPointer t

-- | Alias for address type.
tAddr :: Platform -> Type
tAddr pp = TInt (8 * platformAddrBytes pp)

-- | Alias for natural numner type.
tNat :: Platform -> Type
tNat pp = TInt (8 * platformAddrBytes pp)

-- | Alias for machine integer type.
tInt :: Platform -> Type
tInt pp = TInt (8 * platformAddrBytes pp)

-- | Alias for address type.
tTag :: Platform -> Type
tTag pp = TInt (8 * platformTagBytes  pp)


-- Predicates -----------------------------------------------------------------
-- | Check whether this is the Void# type.
isVoidT :: C.Type A.Name -> Bool
isVoidT (C.TCon (C.TyConBound (C.UPrim (A.NamePrimTyCon A.PrimTyConVoid) _))) = True
isVoidT _ = False


-- | Check whether some type is signed: IntN or FloatN.
isSignedT :: C.Type A.Name -> Bool
isSignedT tt
 = case tt of
        C.TCon (C.TyConBound (C.UPrim (A.NamePrimTyCon n) _))
         -> case n of
                A.PrimTyConInt{}        -> True
                A.PrimTyConFloat{}      -> True
                _                       -> False
        _                               -> False


-- | Check whether some type is unsigned: NatN or WordN
isUnsignedT :: C.Type A.Name -> Bool
isUnsignedT tt
 = case tt of
        C.TCon (C.TyConBound (C.UPrim (A.NamePrimTyCon n) _))
         -> case n of
                A.PrimTyConNat          -> True
                A.PrimTyConTag          -> True
                A.PrimTyConWord{}       -> True
                _                       -> False
        _                               -> False


-- | Check whether some type is an integral type. Nat, IntN or WordN.
isIntegralT :: C.Type A.Name -> Bool
isIntegralT tt
 = case tt of
        C.TCon (C.TyConBound (C.UPrim (A.NamePrimTyCon n) _))
         -> case n of
                A.PrimTyConNat          -> True
                A.PrimTyConInt          -> True
                A.PrimTyConWord{}       -> True
                _                       -> False
        _                               -> False


-- | Check whether some type is an integral type. Nat, IntN or WordN.
isFloatingT :: C.Type A.Name -> Bool
isFloatingT tt
 = case tt of
        C.TCon (C.TyConBound (C.UPrim (A.NamePrimTyCon n) _))
         -> case n of
                A.PrimTyConFloat  _     -> True
                _                       -> False
        _                               -> False

