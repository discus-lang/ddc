
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
import DDC.Core.Exp
import DDC.Core.Sea.Output.Name
import DDC.Type.Compounds


-- | Convert a Sea type to an LlvmType.
toLlvmType :: Platform -> Type Name -> LlvmType
toLlvmType platform tt
 = case tt of
        -- A primitive type.
        TCon tc
          -> llvmTypeOfTyCon platform tc

        -- A pointer to a primitive type.
        TApp (TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConPtr) _))) t2
         -> LMPointer (toLlvmType platform t2)

        -- Function types become pointers to functions.
        TApp{}
         |  (tsArgs, tResult) <- takeTFunArgResult tt
         -> LMPointer $ LMFunction 
         $  FunctionDecl
             { declName          = "dummy.function.name"
             , declLinkage       = Internal
             , declCallConv      = CC_Ccc
             , declReturnType    = toLlvmType platform tResult
             , declParamListType = FixedArgs
             , declParams        = map (llvmParameterOfType platform) tsArgs
             , declAlign         = AlignmentBytes (platformAlignFunctions platform) }


        _ -> die "invalid type"


-- | Convert a Sea TyCon to a LlvmType.
llvmTypeOfTyCon :: Platform -> TyCon Name -> LlvmType
llvmTypeOfTyCon platform tycon
 = case tycon of
        TyConBound (UPrim NameObjTyCon _)
         -> tHeapObj platform

        TyConBound (UPrim (NamePrimTyCon tc) _)
         -> case tc of
                PrimTyConVoid           -> LMVoid
                PrimTyConAddr           -> LMPointer (LMInt 8)
                PrimTyConNat            -> LMInt (platformAddrWidth platform)
                PrimTyConTag            -> LMInt (platformTagWidth  platform)
                PrimTyConBool           -> LMInt 1
                PrimTyConWord bits      -> LMInt bits
                PrimTyConInt  bits      -> LMInt bits
                PrimTyConString         -> LMPointer (LMInt 8)

                PrimTyConFloat bits
                 -> case bits of
                        32              -> LMFloat
                        64              -> LMDouble
                        80              -> LMFloat80
                        128             -> LMFloat128
                        _               -> die "invalid float tycon"

                _                       -> die "invalid prim tycon"

        _ -> die "invalid tycon"


-- | Convert a parameter type to a LlvmParameter decl.
llvmParameterOfType :: Platform -> Type Name -> Parameter
llvmParameterOfType platform tt
        = Parameter
        { parameterType     = toLlvmType platform tt
        , parameterAttrs    = [] }

