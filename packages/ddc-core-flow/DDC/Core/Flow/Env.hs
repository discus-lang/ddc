
module DDC.Core.Flow.Env
        ( primDataDefs
        , primSortEnv
        , primKindEnv
        , primTypeEnv 

        , typeOfFlowOp
        , typeOfDataCon
        , typeOfPrimCast
        , typeOfPrimArith)
where
import DDC.Core.Flow.Name
import DDC.Core.Flow.Compounds
import DDC.Type.DataDef
import DDC.Type.Compounds
import DDC.Type.Transform.LiftT
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


        -- Flow -----------------------------------------------------
        -- Stream
        , DataDef
                (NameDataTyCon DataTyConStream)
                [kRate, kData]
                (Just   [])

        -- Vector
        , DataDef
                (NameDataTyCon DataTyConVector)
                [kRate, kData]
                (Just   [])

        -- Array
        , DataDef
                (NameDataTyCon DataTyConArray)
                [kData]
                (Just   [])

        -- Boxed -----------------------------------------------------
        -- Tuple
        , DataDef
                (NameDataTyCon (DataTyConTuple 2))
                [kData, kData]
                (Just   [ ( NameDataCon (DataConTuple 2)
                          , [tIx kData 1, tIx kData 0]) ])
        ]

-- Sorts ---------------------------------------------------------------------
primSortEnv :: Env Name
primSortEnv  = Env.setPrimFun sortOfPrimName Env.empty


-- | Take the sort of a primitive kind name.
sortOfPrimName :: Name -> Maybe (Sort Name)
sortOfPrimName _
 = Nothing

{-
 case nn of
        NameFlowKiCon FlowKiConNatP     
          -> Just sProp

        NameFlowKiCon FlowKiConRate     
          -> Just sProp

        _ -> Nothing
-}

-- Kinds ----------------------------------------------------------------------
-- | Kind environment containing kinds of primitive data types.
primKindEnv :: Env Name
primKindEnv = Env.setPrimFun kindOfPrimName Env.empty


-- | Take the kind of a primitive name.
--
--   Returns `Nothing` if the name isn't primitive. 
--
kindOfPrimName :: Name -> Maybe (Kind Name)
kindOfPrimName nn
 = case nn of
        NameFlowKiCon FlowKiConNatP
         -> Just sProp

        NameFlowKiCon FlowKiConRate
         -> Just sProp

        -- FlowTyCon
        NameFlowTyCon (FlowTyConNatP _)
         -> Just kNatP

        NameFlowTyCon FlowTyConLen
         -> Just $ kNatP `kFun` kRate

        -- DataTyCon
        NameDataTyCon (DataTyConTuple 2)
         -> Just $ kData `kFun` kData `kFun` kData

        NameDataTyCon DataTyConArray
         -> Just $ kData `kFun` kData

        NameDataTyCon DataTyConVector
         -> Just $ kRate `kFun` kData `kFun` kData

        NameDataTyCon DataTyConStream
         -> Just $ kRate `kFun` kData `kFun` kData

        NameDataTyCon DataTyConSegd
         -> Just $ kRate `kFun` kRate `kFun` kData

        NameDataTyCon (DataTyConSel 1)
         -> Just $ kRate `kFun` kRate `kFun` kData

        NameDataTyCon (DataTyConSel 2)
         -> Just $ kRate `kFun` kRate `kFun` kRate `kFun` kData

        NameDataTyCon DataTyConRef
         -> Just $ kData `kFun` kData

        -- Primitive type constructors.
        NamePrimTyCon tc
         -> Just $ kindOfPrimTyCon tc

        _ -> Nothing


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


-- Types ----------------------------------------------------------------------
-- | Type environment containing types of primitive operators.
primTypeEnv :: Env Name
primTypeEnv = Env.setPrimFun typeOfPrimName Env.empty


-- | Take the type of a name,
--   or `Nothing` if this is not a value name.
typeOfPrimName :: Name -> Maybe (Type Name)
typeOfPrimName dc
 = case dc of
        -- Flow operators
        NameFlowOp p            -> Just $ typeOfFlowOp p

        -- Data constructors
        NameDataCon p           -> Just $ typeOfDataCon p

        -- Primitive operators.
        NamePrimArith p         -> Just $ typeOfPrimArith p
        NamePrimCast p          -> Just $ typeOfPrimCast p

        -- Unboxed Literals
        NameLitBool _           -> Just $ tBoolU
        NameLitNat  _           -> Just $ tNatU
        NameLitInt  _           -> Just $ tIntU
        NameLitWord _ bits      -> Just $ tWordU bits

        _                       -> Nothing


-- | Take the type of a primitive data-flow operator.
typeOfFlowOp :: FlowOp -> Type Name
typeOfFlowOp op
 = case op of
        -- fromStream :: [k : Rate]. [a : Data]
        --            .  Stream k a -> Vector k a
        FlowOpFromStream
         -> tForalls [kRate, kData]
         $  \[tK, tA]
         -> tStream tK tA `tFunPE` tVector tK tA

        -- toStream   :: [k : Rate]. [a : Data]
        --            .  Vector k a -> Stream k a
        FlowOpToStream
         -> tForalls [kRate, kData]
         $  \[tK, tA]
         -> tVector tK tA `tFunPE` tStream tK tA

        -- fromVector :: [k : Rate]. [a : Data]
        --            .  Vector k a -> Array a
        FlowOpFromVector 
         -> tForalls [kRate, kData]
         $  \[tK, tA]
         -> tVector tK tA `tFunPE` tArray tA

        -- mkSel1#    :: [k1 : Rate]. [a : Data]
        --            .  Stream k1 Bool#
        --            -> ([k2 : Rate]. Sel1 k1 k2 -> a)
        --            -> a
        FlowOpMkSel 1
         -> tForalls [kRate, kData]
         $  \[tK1, tA]
         -> tStream tK1 tBoolU
                `tFunPE` (tForall kRate $ \tK2 
                                -> tSel1 (liftT 1 tK1) tK2 `tFunPE` (liftT 1 tA))
                `tFunPE` tA

        -- map   :: [k : Rate] [a b : Data]
        --       .  (a -> b) -> Stream k a -> Stream k b
        FlowOpMap 1
         -> tForalls [kRate, kData, kData]
         $  \[tK, tA, tB]
         -> (tA `tFunPE` tB)
                `tFunPE` tStream tK tA
                `tFunPE` tStream tK tB

        -- rep  :: [n : Nat']. [a : Data]
        --      .  n -> a -> Stream (Len n) a
        FlowOpRep 
         -> tForalls [kNatP, kData]
         $  \[tN, tA]
         ->     tN `tFunPE` tA `tFunPE` tStream (tLen tN) tA

        -- reps  :: [k1 k2 : Rate]. [a : Data]
        --       .  Segd   k1 k2 
        --       -> Stream k1 a
        --       -> Stream k2 a
        FlowOpReps 
         -> tForalls [kRate, kRate, kData]
         $  \[tK1, tK2, tA]
         -> tSegd tK1 tK2
                `tFunPE` tStream tK1 tA
                `tFunPE` tStream tK2 tA

        -- fold :: [k : Rate]. [a b: Data]
        --      .  (a -> b -> a) -> a -> Stream k b -> a
        FlowOpFold    
         -> tForalls [kRate, kData, kData] 
         $  \[tK, tA, tB]
         -> (tA `tFunPE` tB `tFunPE` tA)
                `tFunPE` tA
                `tFunPE` tStream tK tA
                `tFunPE` tA

        -- folds :: [k1 k2 : Rate]. [a b: Data]
        --       .  Segd   k1 k2 
        --       -> (a -> b -> a)       -- fold operator
        --       -> Stream k1 a         -- start values
        --       -> Stream k2 b         -- source elements
        --       -> Stream k1 a         -- result values
        FlowOpFolds
         -> tForalls [kRate, kRate, kData, kData]
         $  \[tK1, tK2, tA, tB]
         -> tSegd tK1 tK2
                `tFunPE` (tA `tFunPE` tB `tFunPE` tA)
                `tFunPE` tStream tK1 tA
                `tFunPE` tStream tK2 tB
                `tFunPE` tStream tK1 tA

        -- pack  :: [k1 k2 : Rate]. [a : Data]
        --       .  Sel2 k1 k2
        --       -> Stream k1 a -> Stream k2 a
        FlowOpPack
         -> tForalls [kRate, kRate, kData]
         $  \[tK1, tK2, tA]
         -> tSel1 tK1 tK2 
                `tFunPE` tStream tK1 tA
                `tFunPE` tStream tK2 tA

        _ -> error "typeOfPrimFlow: not finished"


-- | Take the type of a data constructor.
typeOfDataCon :: DataCon -> Type Name
typeOfDataCon cc
 = case cc of
        DataConTuple 2
         -> tForalls [kData, kData] 
         $ \[t1, t2] -> t1 `tFunPE` t2 `tFunPE` tTuple2 t1 t2

        _ -> error "typeOfDataCon: not finished"


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

