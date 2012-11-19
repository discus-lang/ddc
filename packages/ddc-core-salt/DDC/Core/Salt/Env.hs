
-- | Types of Disciple Core Salt primops.
module DDC.Core.Salt.Env
        ( primDataDefs
        , primKindEnv
        , primTypeEnv
        , typeOfPrimCall
        , typeOfPrimStore 
        , typeIsUnboxed)
where
import DDC.Core.Salt.Compounds
import DDC.Core.Salt.Name
import DDC.Type.DataDef
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Exp
import DDC.Type.Env                             (Env)
import qualified DDC.Type.Env                   as Env


-- DataDefs -------------------------------------------------------------------
-- | Data type definitions for:
--
-- >  Type                        Constructors
-- >  ----                --------------------------
-- >  Bool#               True# False#
-- >  Nat#                0# 1# 2# ...
-- >  Int#                ... -2i# -1i# 0i# 1i# 2i# ...
-- >  Tag#                (none, convert from Nat#)
-- >  Word{8,16,32,64}#   42w8# 123w64# ...
-- >  Float{32,64}#       (none, convert from Int#)
-- 
primDataDefs :: DataDefs Name
primDataDefs
 = fromListDataDefs
        -- Bool
        [ DataDef
                (NamePrimTyCon PrimTyConBool)
                []
                (Just   [ (NameLitBool True,  [])
                        , (NameLitBool False, []) ])
        -- Nat
        , DataDef (NamePrimTyCon PrimTyConNat) [] Nothing

        -- Int
        , DataDef (NamePrimTyCon PrimTyConInt) [] Nothing

        -- Tag
        , DataDef (NamePrimTyCon PrimTyConTag) [] Nothing

        -- Word 8, 16, 32, 64
        , DataDef (NamePrimTyCon (PrimTyConWord 8))  [] Nothing
        , DataDef (NamePrimTyCon (PrimTyConWord 16)) [] Nothing
        , DataDef (NamePrimTyCon (PrimTyConWord 32)) [] Nothing
        , DataDef (NamePrimTyCon (PrimTyConWord 64)) [] Nothing

        -- Float 32, 64
        , DataDef (NamePrimTyCon (PrimTyConFloat 32)) [] Nothing
        , DataDef (NamePrimTyCon (PrimTyConFloat 64)) [] Nothing
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
        NamePrimOp p       -> Just $ typeOfPrim p
        NameLitVoid        -> Just $ tVoid
        NameLitBool _      -> Just $ tBool
        NameLitNat  _      -> Just $ tNat
        NameLitInt  _      -> Just $ tInt
        NameLitWord _ bits -> Just $ tWord bits
        NameLitTag  _      -> Just $ tTag
        _                  -> Nothing


-- | Take the type of a primitive.
typeOfPrim :: PrimOp -> Type Name
typeOfPrim pp
 = case pp of
        PrimArith    op -> typeOfPrimArith    op
        PrimCast     cc -> typeOfPrimCast     cc
        PrimCall     pc -> typeOfPrimCall     pc
        PrimControl  pc -> typeOfPrimControl  pc
        PrimStore    ps -> typeOfPrimStore    ps


-- PrimOps --------------------------------------------------------------------
-- | Take the type of a primitive operator.
typeOfPrimArith :: PrimArith -> Type Name
typeOfPrimArith op
 = case op of
        -- Numeric
        PrimArithNeg    -> tForall kData $ \t -> t `tFunPE` t
        PrimArithAdd    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithSub    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithMul    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithDiv    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithRem    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t

        -- Comparison
        PrimArithEq     -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBool
        PrimArithNeq    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBool
        PrimArithGt     -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBool
        PrimArithLt     -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBool
        PrimArithLe     -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBool
        PrimArithGe     -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBool

        -- Boolean
        PrimArithAnd    -> tBool `tFunPE` tBool `tFunPE` tBool
        PrimArithOr     -> tBool `tFunPE` tBool `tFunPE` tBool

        -- Bitwise
        PrimArithShl    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithShr    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithBAnd   -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithBOr    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithBXOr   -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t


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
        PrimStoreSize 
         -> tForall kData $ \_ -> tNat

        PrimStoreSize2
         -> tForall kData $ \_ -> tNat

        PrimStoreCreate
         -> tNat `tFunPE` tVoid

        PrimStoreCheck
         -> tNat `tFunPE` tBool

        PrimStoreRecover
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


-------------------------------------------------------------------------------
-- | Check if a type is an unboxed data type.
typeIsUnboxed :: Type Name -> Bool
typeIsUnboxed tt
 = case tt of
        TVar{}          -> False

        -- All plain constructors are unboxed.
        TCon (TyConBound _ k)
         | isDataKind k -> True

        TCon _          -> False

        TForall _ t     -> typeIsUnboxed t

        -- Pointers to objects are boxed.
        TApp{}
         | Just (_tR, tTarget)  <- takeTPtr tt
         , tTarget == tObj
         -> False

        TApp t1 t2      
         -> typeIsUnboxed t1 || typeIsUnboxed t2

        TSum{}          -> False


