
-- | Convert Sea types to LLVM types.
module DDC.Core.Llvm.Convert.Type
        ( toLlvmType
        , llvmTypeOfTyCon
        , llvmParameterOfType)
where
import DDC.Llvm.Attr
import DDC.Llvm.Type
import DDC.Core.Llvm.Runtime.Object
import DDC.Core.Llvm.Platform
import DDC.Core.Llvm.LlvmM
import DDC.Core.Sea.Output.Name
import DDC.Type.Compounds
import qualified DDC.Core.Exp   as C


-- | Convert a Sea type to an LlvmType.
toLlvmType :: Platform -> C.Type Name -> Type
toLlvmType platform tt
 = case tt of
        -- A primitive type.
        C.TCon tc
          -> llvmTypeOfTyCon platform tc

        -- A pointer to a primitive type.
        C.TApp (C.TCon (C.TyConBound (C.UPrim (NamePrimTyCon PrimTyConPtr) _))) t2
         -> TPointer (toLlvmType platform t2)

        -- Function types become pointers to functions.
        C.TApp{}
         |  (tsArgs, tResult) <- takeTFunArgResult tt
         -> TPointer $ TFunction 
         $  FunctionDecl
             { declName          = "dummy.function.name"
             , declLinkage       = Internal
             , declCallConv      = CC_Ccc
             , declReturnType    = toLlvmType platform tResult
             , declParamListType = FixedArgs
             , declParams        = map (llvmParameterOfType platform) tsArgs
             , declAlign         = AlignBytes (platformFunctionAlignBytes platform) }


        _ -> die ("invalid type " ++ show tt)


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
                PrimTyConWord bits      -> TInt bits
                PrimTyConInt  bits      -> TInt bits
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


-- | Convert a parameter type to a LlvmParameter decl.
llvmParameterOfType :: Platform -> C.Type Name -> Param
llvmParameterOfType platform tt
        = Param
        { paramType     = toLlvmType platform tt
        , paramAttrs    = [] }

