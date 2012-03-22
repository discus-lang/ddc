
-- | Types of SeaOutput primops.
module DDC.Core.Sea.Output.Env
        ( primDataDefs
        , primKindEnv
        , primTypeEnv

        , tObj
        , tPtr
        , tAddr, tNat, tTag, tBool
        , tInt)
where
import DDC.Core.DataDef
import DDC.Core.Sea.Output.Name
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Env                             (Env)
import qualified DDC.Type.Env                   as Env


-- DataDefs -------------------------------------------------------------------
-- | Data type definitions for:
--
-- @  Type   Constructors
--  ----   ------------
--  Nat    ... -2 -1 0 1 2 ...
--  Tag    0 1 2 ...
-- @
primDataDefs :: DataDefs Name
primDataDefs
 = fromListDataDefs
        -- Nat
        [ DataDef
                (NamePrimTyCon PrimTyConNat)
                []
                Nothing

        , DataDef
                (NamePrimTyCon PrimTyConTag)
                []
                Nothing

        , DataDef
                (NamePrimTyCon PrimTyConBool)
                []
                (Just   [ (NameBool True,  [])
                        , (NameBool False, []) ])
        ]


-- Kinds ----------------------------------------------------------------------
-- | Kind environment containing kinds of primitive data types.
primKindEnv :: Env Name
primKindEnv = Env.setPrimFun kindOfName Env.empty


-- | Take the kind of a name, 
--   or `Nothing` if this is not a type name.
kindOfName :: Name -> Maybe (Kind Name)
kindOfName nn
 = case nn of
        NameObjTyCon      -> Just $ kData
        NamePrimTyCon tc  -> Just $ kindOfPrimTyCon tc
        NamePrim _        -> Just $ kData
        NameNat  _        -> Just $ kData
        _                 -> Nothing


-- | Take the kind of a primitive name.
--
--   Returns `Nothing` if the name isn't primitive. 
--
kindOfPrimTyCon :: PrimTyCon -> Kind Name
kindOfPrimTyCon tc
 = case tc of
        PrimTyConVoid    -> kData
        PrimTyConPtr     -> (kData `kFun` kData)
        PrimTyConAddr    -> kData
        PrimTyConNat     -> kData
        PrimTyConTag     -> kData
        PrimTyConBool    -> kData
        PrimTyConWord  _ -> kData
        PrimTyConInt   _ -> kData
        PrimTyConFloat _ -> kData
        PrimTyConString  -> kData


-- Types ----------------------------------------------------------------------
-- | Type environment containing types of primitive operators.
primTypeEnv :: Env Name
primTypeEnv = Env.setPrimFun typeOfName Env.empty


-- | Take the type of a name,
--   or `Nothing` if this is not a value name.
typeOfName :: Name -> Maybe (Type Name)
typeOfName nn
 = case nn of
        NamePrim p      -> Just $ typeOfPrim p
        NameNat  _      -> Just $ tNat
        NameTag  _      -> Just $ tTag
        NameBool _      -> Just $ tBool
        _               -> Nothing


-- | Take the type of a primitive.
typeOfPrim :: Prim -> Type Name
typeOfPrim pp
 = case pp of
        PrimOp       op -> typeOfPrimOp       op
        PrimCast     cc -> typeOfPrimCast     cc
        PrimCall     pc -> typeOfPrimCall     pc
        PrimControl  pc -> typeOfPrimControl  pc
        PrimStore    ps -> typeOfPrimStore    ps
        PrimExternal ps -> typeOfPrimExternal ps


-- Shorthands -----------------------------------------------------------------
tObj, tAddr, tTag, tNat, tBool, tString :: Type Name
tObj      = TCon (TyConBound (UPrim  NameObjTyCon                   kData))
tVoid     = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConVoid)   kData))
tAddr     = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConAddr)   kData))
tTag      = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConTag)    kData))
tNat      = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConNat)    kData))
tBool     = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConBool)   kData))
tString   = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConString) kData))


tPtr :: Type Name -> Type Name
tPtr t    = TApp (TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConPtr)  (kFun kData kData))))
                 t

tInt :: Int -> Type Name
tInt bits = TCon (TyConBound (UPrim (NamePrimTyCon (PrimTyConInt bits)) kData))



-- PrimOp ---------------------------------------------------------------------
-- | Take the type of a primitive operator.
typeOfPrimOp :: PrimOp -> Type Name
typeOfPrimOp op
 = case op of
        -- Arithmetic
        PrimOpNeg       -> tForall kData $ \t -> t `tFunPE` t
        PrimOpAdd       -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimOpSub       -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimOpMul       -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimOpDiv       -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimOpMod       -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t

        -- Comparison
        PrimOpEq        -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBool
        PrimOpNeq       -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBool
        PrimOpGt        -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBool
        PrimOpLt        -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBool
        PrimOpLe        -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBool
        PrimOpGe        -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBool

        -- Boolean
        PrimOpAnd       -> tBool `tFunPE` tBool `tFunPE` tBool
        PrimOpOr        -> tBool `tFunPE` tBool `tFunPE` tBool


-- PrimCast -------------------------------------------------------------------
-- | Take the type of a primitive cast.
typeOfPrimCast :: PrimCast -> Type Name
typeOfPrimCast cc
 = case cc of
        PrimCastOp      
         -> tForalls [kData, kData] $ \[t1, t2] -> t1 `tFunPE` t2

        PrimCastNatToInt bits
         -> tNat `tFunPE` tInt bits


-- PrimCall -------------------------------------------------------------------
-- | Take the type of a primitive call operator.
typeOfPrimCall :: PrimCall -> Type Name
typeOfPrimCall cc
 = case cc of
        PrimCallTail    arity       -> makePrimCallType    arity
        PrimCallPartial arity args  -> makePrimPartialType arity args
        PrimCallApply   arity       -> makePrimApplyType   arity
        PrimCallForce               -> tFunPE (tPtr tObj) (tPtr tObj)


-- | Make the type of the @callN#@ and @tailcallN@ primitives.
makePrimCallType :: Int -> Type Name
makePrimCallType arity
 = let  tSuper   = foldr tFunPE 
                         (TVar (UIx 0 kData))
                         (reverse [TVar (UIx i kData) | i <- [1..arity]])

        tCall    = foldr TForall (tSuper `tFunPE` tSuper) 
                         [BAnon k | k <- replicate (arity + 1) kData]

   in   tCall


-- | Make the type of a @partialN@ primitive.
makePrimPartialType :: Int -> Int -> Type Name
makePrimPartialType args arity
 = let  tSuper  = foldr tFunPE 
                        (tPtr tObj)
                        (replicate arity (tPtr tObj))

        tOp     = foldr tFunPE
                        (tPtr tObj)
                        (replicate args  (tPtr tObj))

   in   tSuper `tFunPE` tOp


-- | Make the type of an @apply@ primitive.
makePrimApplyType :: Int -> Type Name
makePrimApplyType arity
 = let  tCall   = foldr tFunPE
                        (tPtr tObj)
                        (replicate arity (tPtr tObj))

   in   tPtr tObj `tFunPE` tCall


-- PrimControl ----------------------------------------------------------------
typeOfPrimControl :: PrimControl -> Type Name
typeOfPrimControl pc
 = case pc of
        PrimControlFail         -> tForall kData $ \t -> t
        PrimControlReturn       -> tForall kData $ \t -> t `tFunPE` t


-- PrimStore ------------------------------------------------------------------
-- | Take the type of a primitive projection.
typeOfPrimStore :: PrimStore -> Type Name
typeOfPrimStore jj
 = case jj of
        PrimStoreRead        
         -> tForall kData $ \t -> tPtr t `tFunPE` t

        PrimStoreWrite
         -> tForall kData $ \t -> tPtr t `tFunPE` t `tFunPE` tVoid

        PrimStoreProjTag
         -> tPtr tObj `tFunPE` tTag

        PrimStoreProjField PrimStoreLayoutRaw  
         -> tForall kData $ \t -> tNat `tFunPE` tPtr tObj `tFunPE` tPtr t

        PrimStoreAllocData PrimStoreLayoutRaw
         -> tTag `tFunPE` tNat `tFunPE` tPtr tObj

        _ -> error "typeOfPrimStore: sorry"


-- PrimExternal --------------------------------------------------------------
typeOfPrimExternal :: PrimExternal -> Type Name
typeOfPrimExternal ps
 = case ps of
        PrimExternalPutStr        -> tString   `tFunPE` tVoid
        PrimExternalPutStrLn      -> tString   `tFunPE` tVoid
        PrimExternalShowInt bits  -> tInt bits `tFunPE` tString

