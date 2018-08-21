
-- | Convert Salt types to LLVM types.
module DDC.Core.Llvm.Convert.Type
        ( -- * Type conversion.
          convertType
        , convertSuperType
        , importedFunctionDeclOfType

          -- * Builtin Types
        , tObj, sObj,  aObj
        , tPtr, tAddr, tNat, tInt, tTag

          -- * Type Constructors
        , convTyCon

          -- * Predicates
        , isVoidT
        , isBoolT
        , isSignedT
        , isUnsignedT
        , isIntegralT
        , isFloatingT)
where
import DDC.Core.Llvm.Convert.Base
import DDC.Llvm.Syntax.Type
import DDC.Llvm.Syntax.Attr
import DDC.Core.Salt.Platform
import DDC.Type.Env
import DDC.Type.Exp.Simple.Compounds
import DDC.Type.Exp.Simple.Predicates
import DDC.Data.Pretty
import qualified DDC.Core.Salt          as A
import qualified DDC.Core.Salt.Name     as A
import qualified DDC.Core.Module        as C
import qualified DDC.Core.Exp           as C
import qualified DDC.Type.Env           as Env
import qualified Data.Text              as T
import Control.Monad

-- Type -----------------------------------------------------------------------
-- | Convert a Salt type to an LlvmType.
convertType :: Platform -> KindEnv A.Name -> C.Type A.Name -> ConvertM Type
convertType pp kenv tt
 = case tt of
        -- A polymorphic type,
        -- represented as a generic boxed object.
        C.TVar u
         -> case Env.lookup u kenv of
             Nothing
              -> throw $ ErrorInvalidBound u
                       $ Just "Type variable not in kind environment."

             Just k
              | isDataKind k
              -> return $ TPointer (tObj pp)

              | otherwise
              -> throw $ ErrorInvalidBound u
                       $ Just "Bound type variable does not have kind Data."

        -- A primitive type.
        C.TCon tc
          -> convTyCon pp tc

        -- A pointer to a primitive type.
        C.TApp{}
         | Just (A.NamePrimTyCon A.PrimTyConPtr, [_r, t2])
                <- takeNameTyConApps tt
         -> do  t2'     <- convertType pp kenv t2
                return  $ TPointer t2'

        -- Function types become pointers to functions.
        C.TApp{}
         -> do  (tsArgs, tResult)    <- convertSuperType pp kenv tt
                return
                  $ TPointer $ TFunction
                  $ FunctionDecl
                  { declName             = "dummy.function.name"
                  , declLinkage          = Internal
                  , declCallConv         = CC_Ccc
                  , declReturnType       = tResult
                  , declParamListType    = FixedArgs
                  , declParams           = [Param t [] | t <- tsArgs]
                  , declAlign            = AlignBytes (platformAlignBytes pp)
                  , declGarbageCollector = Just "shadow-stack" }

        C.TForall b t
         -> let kenv'   = Env.extend b kenv
            in  convertType pp kenv' t

        _ -> throw $ ErrorInvalidType tt
                   $ Just "Cannot convert type."


-- Super Type -----------------------------------------------------------------
-- | Split the parameter and result types from a supercombinator type and
--   and convert them to LLVM form.
--
--   We can't split the type first and just call 'convertType' above as we need
--   to decend into any quantifiers that wrap the body type.
convertSuperType
        :: Platform
        -> KindEnv A.Name
        -> C.Type  A.Name
        -> ConvertM ([Type], Type)

convertSuperType pp kenv tt
 = case tt of
        C.TApp{}
         |  (_, tsArgs, tResult) <- takeTFunWitArgResult tt
         ,  not $ null tsArgs
         -> do  tsArgs'   <- mapM (convertType pp kenv) tsArgs
                tResult'  <- convertType pp kenv tResult
                return (tsArgs', tResult')

        C.TForall b t
         -> let kenv' = Env.extend b kenv
            in  convertSuperType pp kenv' t

        _ -> throw $ ErrorInvalidType tt
                   $ Just $ "Cannot use this as the type of a super."
                          ++ show (takeTFunArgResult tt)


-- Imports --------------------------------------------------------------------
-- | Convert an imported function type to a LLVM declaration.
importedFunctionDeclOfType
        :: Platform
        -> KindEnv A.Name
        -> C.ImportValue A.Name (C.Type A.Name)
        -> Maybe (C.ExportValue A.Name (C.Type A.Name))
        -> A.Name
        -> C.Type A.Name
        -> Maybe (ConvertM FunctionDecl)

importedFunctionDeclOfType pp kenv isrc mesrc nSuper tt

 | C.ImportValueModule{} <- isrc
 = Just $ do
        let Just strName
                = liftM renderPlain
                $ A.seaNameOfSuper (Just isrc) mesrc nSuper

        (tsArgs, tResult)       <- convertSuperType pp kenv tt
        let mkParam t           = Param t []
        return  $ FunctionDecl
                { declName              = strName
                , declLinkage           = External
                , declCallConv          = CC_Ccc
                , declReturnType        = tResult
                , declParamListType     = FixedArgs
                , declParams            = map mkParam tsArgs
                , declAlign             = AlignBytes (platformAlignBytes pp)
                , declGarbageCollector  = Just "shadow-stack" }

 | C.ImportValueSea _ _ strName _  <- isrc
 = Just $ do
        (tsArgs, tResult)       <- convertSuperType pp kenv tt
        let mkParam t           = Param t []
        return  $ FunctionDecl
                { declName              = T.unpack strName
                , declLinkage           = External
                , declCallConv          = CC_Ccc
                , declReturnType        = tResult
                , declParamListType     = FixedArgs
                , declParams            = map mkParam tsArgs
                , declAlign             = AlignBytes (platformAlignBytes pp)
                , declGarbageCollector  = Nothing }


-- TyCon ----------------------------------------------------------------------
-- | Convert a Sea TyCon to a LlvmType.
convTyCon :: Platform -> C.TyCon A.Name -> ConvertM Type
convTyCon platform tycon
 = case tycon of
        C.TyConSpec  C.TcConUnit
         -> return $ TPointer (tObj platform)

        C.TyConBound A.NameObjTyCon
         -> return $ tObj platform

        C.TyConBound (A.NamePrimTyCon tc)
         -> case tc of
             A.PrimTyConVoid      -> return $ TVoid
             A.PrimTyConBool      -> return $ TInt 1
             A.PrimTyConNat       -> return $ TInt (8 * platformAddrBytes platform)
             A.PrimTyConInt       -> return $ TInt (8 * platformAddrBytes platform)
             A.PrimTyConWord bits -> return $ TInt (fromIntegral bits)
             A.PrimTyConTag       -> return $ TInt (8 * platformTagBytes  platform)
             A.PrimTyConAddr      -> return $ TInt (8 * platformAddrBytes platform)

             A.PrimTyConFloat bits
              -> case bits of
                        32        -> return TFloat
                        64        -> return TDouble
                        80        -> return TFloat80
                        128       -> return TFloat128

                        _ -> throw $ ErrorInvalidTyCon tycon
                                   $ Just "Float has a non-standard width."

             -- Text literals are represented as pointers to the static text data.
             A.PrimTyConTextLit   -> return $ tPtr (TInt 8)

             _ -> throw $ ErrorInvalidTyCon tycon
                        $ Just "Not a primitive type constructor."

        _ -> throw $ ErrorInvalidTyCon tycon
                   $ Just "Cannot convert type constructor."


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
isVoidT (C.TCon (C.TyConBound (A.NamePrimTyCon A.PrimTyConVoid)))
          = True
isVoidT _ = False


-- | Check whether this is the Bool# type.
isBoolT :: C.Type A.Name -> Bool
isBoolT (C.TCon (C.TyConBound (A.NamePrimTyCon A.PrimTyConBool)))
          = True
isBoolT _ = False


-- | Check whether some type is signed: IntN or FloatN.
isSignedT :: C.Type A.Name -> Bool
isSignedT tt
 = case tt of
        C.TCon (C.TyConBound (A.NamePrimTyCon tc))
          -> A.primTyConIsSigned tc
        _ -> False


-- | Check whether some type is unsigned: NatN or WordN
isUnsignedT :: C.Type A.Name -> Bool
isUnsignedT tt
 = case tt of
        C.TCon (C.TyConBound (A.NamePrimTyCon tc))
          -> A.primTyConIsUnsigned tc
        _ -> False


-- | Check whether some type is an integral type. Nat, Int, WordN or Addr
isIntegralT :: C.Type A.Name -> Bool
isIntegralT tt
 = case tt of
        C.TCon (C.TyConBound (A.NamePrimTyCon tc))
          -> A.primTyConIsIntegral tc
        _ -> False


-- | Check whether some type is an integral type. Nat, IntN or WordN.
isFloatingT :: C.Type A.Name -> Bool
isFloatingT tt
 = case tt of
        C.TCon (C.TyConBound (A.NamePrimTyCon tc))
          -> A.primTyConIsFloating tc
        _ -> False

