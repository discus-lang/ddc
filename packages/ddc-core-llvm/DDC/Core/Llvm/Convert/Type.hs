
-- | Convert Sea types to LLVM types.
module DDC.Core.Llvm.Convert.Type
        ( -- * Type conversion.
          convTypeM
        , convType
        , llvmParameterOfType

          -- * Builtin Types
        , tObj, sObj, aObj
        , tPtr

          -- * Type Constructors
        , llvmTypeOfTyCon

          -- * Structure definitions
        , convStruct
        , convField)

where
import DDC.Llvm.Attr
import DDC.Llvm.Type
import DDC.Core.Llvm.Platform
import DDC.Core.Llvm.LlvmM
import DDC.Core.Sea.Output.Name
import DDC.Type.Compounds
import Control.Monad.State.Strict
import qualified DDC.Core.Exp   as C


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
          -> llvmTypeOfTyCon platform tc

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
             , declAlign         = AlignBytes (platformFunctionAlignBytes platform) }


        _ -> die ("invalid type " ++ show tt)

-- | Convert a parameter type to a LlvmParameter decl.
llvmParameterOfType :: Platform -> C.Type Name -> Param
llvmParameterOfType platform tt
        = Param
        { paramType     = convType platform tt
        , paramAttrs    = [] }


-- TyCon ----------------------------------------------------------------------
-- | Convert a Sea TyCon to a LlvmType.
llvmTypeOfTyCon :: Platform -> C.TyCon Name -> Type
llvmTypeOfTyCon platform tycon
 = case tycon of
        C.TyConBound (C.UPrim NameObjTyCon _)
         -> tObj platform

        C.TyConBound (C.UPrim (NamePrimTyCon tc) _)
         -> case tc of
                PrimTyConVoid           -> TVoid
                PrimTyConAddr           -> TPointer (TInt 8)
                PrimTyConNat            -> TInt (8 * platformAddrBytes platform)
                PrimTyConTag            -> TInt (8 * platformTagBytes  platform)
                PrimTyConBool           -> TInt 1
                PrimTyConWord bits      -> TInt (fromIntegral bits)
                PrimTyConInt  bits      -> TInt (fromIntegral bits)
                PrimTyConString         -> TPointer (TInt 8)

                PrimTyConFloat bits
                 -> case bits of
                        32              -> TFloat
                        64              -> TDouble
                        80              -> TFloat80
                        128             -> TFloat128
                        _               -> die "invalid float tycon"

                _                       -> die "invalid prim tycon"

        _ -> die "invalid tycon"


-- | Type of Heap objects.
sObj, tObj :: Platform -> Type
sObj platform   = TStruct [TInt (platformHeaderBytes platform * 8)]
tObj platform   = TAlias (aObj platform)

aObj :: Platform -> TypeAlias
aObj platform   = TypeAlias "s.Obj" (sObj platform)


-- | Alias for pointer type.
tPtr :: Type -> Type
tPtr t = TPointer t


-- Struct ---------------------------------------------------------------------
-- | Convert a Structure definition to an LLVM type.
convStruct :: Struct -> Type
convStruct (Struct _ fields)
 = TStruct $ map convField fields


-- | Convert a field definition to an LLVM type.
convField :: Field -> Type
convField ff
 = case ff of
        Field _ t       -> t
        Pad   bytes     -> TInt (8 * bytes)

