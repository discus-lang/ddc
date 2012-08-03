
-- | Types of Disciple Core Salt primops.
module DDC.Core.Salt.Env
        ( primDataDefs
        , primKindEnv
        , primTypeEnv
        , typeOfPrimCall)
where
import DDC.Core.Salt.Compounds
import DDC.Core.Salt.Name
import DDC.Type.DataDef
import DDC.Type.Compounds
import DDC.Type.Exp
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
        -- Bool
        [ DataDef
                (NamePrimTyCon PrimTyConBool)
                []
                (Just   [ (NameBool True,  [])
                        , (NameBool False, []) ])
        -- Nat
        , DataDef (NamePrimTyCon PrimTyConNat) [] Nothing

        -- Int
        , DataDef (NamePrimTyCon PrimTyConInt) [] Nothing

        -- Word 8, 16, 32, 64
        , DataDef (NamePrimTyCon (PrimTyConWord 8))  [] Nothing
        , DataDef (NamePrimTyCon (PrimTyConWord 16)) [] Nothing
        , DataDef (NamePrimTyCon (PrimTyConWord 32)) [] Nothing
        , DataDef (NamePrimTyCon (PrimTyConWord 64)) [] Nothing

        -- Float 32, 64
        , DataDef (NamePrimTyCon (PrimTyConFloat 32)) [] Nothing
        , DataDef (NamePrimTyCon (PrimTyConFloat 64)) [] Nothing

        -- Tag
        , DataDef (NamePrimTyCon PrimTyConTag) [] Nothing
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
        _                 -> Nothing


-- | Take the kind of a primitive name.
--
--   Returns `Nothing` if the name isn't primitive. 
--
kindOfPrimTyCon :: PrimTyCon -> Kind Name
kindOfPrimTyCon tc
 = case tc of
        PrimTyConVoid    -> kData
        PrimTyConBool    -> kData
        PrimTyConNat     -> kData
        PrimTyConInt     -> kData
        PrimTyConWord  _ -> kData
        PrimTyConFloat _ -> kData
        PrimTyConAddr    -> kData
        PrimTyConPtr     -> (kRegion `kFun` kData `kFun` kData)
        PrimTyConTag     -> kData
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
        NameVoid        -> Just $ tVoid
        NameBool _      -> Just $ tBool
        NameNat  _      -> Just $ tNat
        NameInt  _      -> Just $ tInt
        NameWord _ bits -> Just $ tWord bits
        NameTag  _      -> Just $ tTag
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


-- PrimOps --------------------------------------------------------------------
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
        PrimOpRem       -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t

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

        -- Bitwise
        PrimOpShl       -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimOpShr       -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimOpBAnd      -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimOpBOr       -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimOpBXOr      -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t


-- PrimCast -------------------------------------------------------------------
-- | Take the type of a primitive cast.
typeOfPrimCast :: PrimCast -> Type Name
typeOfPrimCast cc
 = case cc of
        PrimCastPromote
         -> tForalls [kData, kData] $ \[t1, t2] -> t2 `tFunPE` t1

        PrimCastTruncate
         -> tForalls [kData, kData] $ \[t1, t2] -> t2 `tFunPE` t1


-- PrimCall -------------------------------------------------------------------
-- | Take the type of a primitive call operator.
typeOfPrimCall :: PrimCall -> Type Name
typeOfPrimCall cc
 = case cc of
        PrimCallTail    arity       -> makePrimCallType    arity


-- | Make the type of the @callN#@ and @tailcallN@ primitives.
makePrimCallType :: Int -> Type Name
makePrimCallType arity
 = let  tSuper   = foldr tFunPE 
                         (TVar (UIx 0))
                         (reverse [TVar (UIx i) | i <- [1..arity]])

        tCall    = foldr TForall (tSuper `tFunPE` tSuper) 
                         [BAnon k | k <- replicate (arity + 1) kData]

   in   tCall


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
        PrimStoreBytesNat
         -> tVoid `tFunPE` tNat

        PrimStoreShiftNat       
         -> tVoid `tFunPE` tNat

        PrimStoreCreate
         -> tNat `tFunPE` tVoid

        PrimStoreAlloc
         -> tNat `tFunPE` tAddr

        PrimStoreRead           
         -> tForall kData $ \t -> tAddr  `tFunPE` tNat `tFunPE` t

        PrimStoreWrite
         -> tForall kData $ \t -> tAddr  `tFunPE` tNat `tFunPE` t `tFunPE` tVoid

        PrimStorePlusAddr
         -> tAddr  `tFunPE` tNat `tFunPE` tAddr

        PrimStoreMinusAddr
         -> tAddr  `tFunPE` tNat `tFunPE` tAddr

        PrimStorePeek
         -> tForalls [kRegion, kData] $ \[r,t] -> tPtr r t `tFunPE` tNat `tFunPE` t

        PrimStorePoke
         -> tForalls [kRegion, kData] $ \[r,t] -> tPtr r t `tFunPE` tNat `tFunPE` t `tFunPE` tVoid

        PrimStorePlusPtr
         -> tForalls [kRegion, kData] $ \[r,t] -> tPtr r t `tFunPE` tNat `tFunPE` tPtr r t

        PrimStoreMinusPtr
         -> tForalls [kRegion, kData] $ \[r,t] -> tPtr r t `tFunPE` tNat `tFunPE` tPtr r t

        PrimStoreMakePtr
         -> tForalls [kRegion, kData] $ \[r,t] -> tAddr `tFunPE` tPtr r t

        PrimStoreTakePtr
         -> tForalls [kRegion, kData] $ \[r,t] -> tPtr r t `tFunPE` tAddr

        PrimStoreCastPtr
         -> tForalls [kRegion, kData, kData] $ \[r,t1,t2] -> tPtr r t2 `tFunPE` tPtr r t1



-- PrimExternal --------------------------------------------------------------
typeOfPrimExternal :: PrimExternal -> Type Name
typeOfPrimExternal ps
 = case ps of
        PrimExternalPutStr        -> tString   `tFunPE` tVoid
        PrimExternalPutStrLn      -> tString   `tFunPE` tVoid
        PrimExternalShowInt       -> tInt      `tFunPE` tString

