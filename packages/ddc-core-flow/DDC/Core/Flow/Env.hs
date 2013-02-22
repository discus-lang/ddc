
module DDC.Core.Flow.Env
        ( primDataDefs
        , primKindEnv
        , primTypeEnv )
where
import DDC.Core.Flow.Name
import DDC.Core.Flow.Compounds
import DDC.Type.DataDef
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Type.Env             (Env)
import qualified DDC.Type.Env   as Env


-- DataDefs -------------------------------------------------------------------
-- | Data type definitions 
--
-- >  Type                         Constructors
-- >  ----                ------------------------------
-- >  Bool#               True# False#
-- >  Nat#                0# 1# 2# ...
-- >  Int#                ... -2i# -1i# 0i# 1i# 2i# ...
-- >  Tag#                (none, convert from Nat#)
-- >  Word{8,16,32,64}#   42w8# 123w64# ...
-- >  Float{32,64}#       (none, convert from Int#)
-- >
-- >  Stream              (none, abstract)
-- >  Vector              (none, abstract)
-- 
primDataDefs :: DataDefs Name
primDataDefs
 = fromListDataDefs
        -- Unboxed --------------------------------------------------
        -- We need these so that we can match against unboxed patterns
        -- in case expressions.
        -- Bool#
        [ DataDef (NamePrimTyCon PrimTyConBool) 
                [] 
                (Just   [ (NameLitBool True,  []) 
                        , (NameLitBool False, []) ])

        -- Nat#
        , DataDef (NamePrimTyCon PrimTyConNat)  [] Nothing

        -- Int#
        , DataDef (NamePrimTyCon PrimTyConInt)  [] Nothing

        -- WordN#
        , DataDef (NamePrimTyCon (PrimTyConWord 64)) [] Nothing
        , DataDef (NamePrimTyCon (PrimTyConWord 32)) [] Nothing
        , DataDef (NamePrimTyCon (PrimTyConWord 16)) [] Nothing
        , DataDef (NamePrimTyCon (PrimTyConWord 8))  [] Nothing


        -- Boxed ----------------------------------------------------
        -- Stream
        , DataDef
                (NameDataTyCon DataTyConStream)
                [kRegion, kClosure, kData]
                (Just   [])

        -- Vector
        , DataDef
                (NameDataTyCon DataTyConVector)
                [kRegion, kData]
                (Just   [])

        ]


-- Kinds ----------------------------------------------------------------------
-- | Kind environment containing kinds of primitive data types.
primKindEnv :: Env Name
primKindEnv = Env.setPrimFun kindOfPrimName Env.empty


-- | Take the kind of a primitive name.
kindOfPrimTyCon :: PrimTyCon -> Kind Name
kindOfPrimTyCon tc
 = case tc of
        PrimTyConVoid    -> kData
        PrimTyConPtr     -> (kRegion `kFun` kData `kFun` kData)
        PrimTyConAddr    -> kData
        PrimTyConBool    -> kData
        PrimTyConNat     -> kData
        PrimTyConInt     -> kData
        PrimTyConWord  _ -> kData
        PrimTyConFloat _ -> kData
        PrimTyConTag     -> kData
        PrimTyConString  -> kData


-- | Take the kind of a primitive name.
--
--   Returns `Nothing` if the name isn't primitive. 
--
kindOfPrimName :: Name -> Maybe (Kind Name)
kindOfPrimName nn
 = case nn of
        -- Stream
        NameDataTyCon DataTyConStream
         -> Just $ kRegion `kFun` kClosure `kFun` kData `kFun` kData

        -- Vector
        NameDataTyCon DataTyConVector
         -> Just $ kRegion `kFun` kData `kFun` kData

        -- Primitive type constructors.
        NamePrimTyCon tc
         -> Just $ kindOfPrimTyCon tc

        _ -> Nothing


-- Types ----------------------------------------------------------------------
-- | Type environment containing types of primitive operators.
primTypeEnv :: Env Name
primTypeEnv = Env.setPrimFun typeOfPrimName Env.empty


-- | Take the type of a name,
--   or `Nothing` if this is not a value name.
typeOfPrimName :: Name -> Maybe (Type Name)
typeOfPrimName dc
 = case dc of
        -- Primitive arithmetic operators
        NamePrimArith p         -> Just $ typeOfPrimArith p
        NamePrimCast p          -> Just $ typeOfPrimCast p
        NamePrimFlow p          -> Just $ typeOfPrimFlow p

        -- Unboxed Literals
        NameLitBool _           -> Just $ tBoolU
        NameLitNat  _           -> Just $ tNatU
        NameLitInt  _           -> Just $ tIntU
        NameLitWord _ bits      -> Just $ tWordU bits

        _                       -> Nothing


-- | Take the type of a primitive cast.
typeOfPrimCast :: PrimCast -> Type Name
typeOfPrimCast cc
 = case cc of
        PrimCastPromote
         -> tForalls [kData, kData] $ \[t1, t2] -> t2 `tFunPE` t1

        PrimCastTruncate
         -> tForalls [kData, kData] $ \[t1, t2] -> t2 `tFunPE` t1


-- | Take the type of a primitive arithmetic operator.
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
        PrimArithEq     -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBoolU
        PrimArithNeq    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBoolU
        PrimArithGt     -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBoolU
        PrimArithLt     -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBoolU
        PrimArithLe     -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBoolU
        PrimArithGe     -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBoolU

        -- Boolean
        PrimArithAnd    -> tBoolU `tFunPE` tBoolU `tFunPE` tBoolU
        PrimArithOr     -> tBoolU `tFunPE` tBoolU `tFunPE` tBoolU

        -- Bitwise
        PrimArithShl    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithShr    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithBAnd   -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithBOr    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithBXOr   -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t


-- | Take the type of a primitive data-flow operator.
typeOfPrimFlow :: PrimFlow -> Type Name
typeOfPrimFlow op
 = case op of
        -- fold :: [a b: *]. [s : %]. [k : $]
        --      .  (a -> b -> a) -> a -> Stream s k b -> a
        PrimFlowFold    
         -> tForalls [kData, kData, kRegion, kClosure] 
         $  \[tA, tB, tS, tK] 
         ->     (tA `tFunPE` tB `tFunPE` tA)
                `tFunPE` tA
                `tFunPE` tStream tS tK tA
                `tFunPE` tA

        _ -> error "typeOfPrimFlow: not finished"

