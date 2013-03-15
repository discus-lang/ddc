
module DDC.Core.Flow.Env
        ( primDataDefs
        , primSortEnv
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
        NameDataTyCon DataTyConStream
         -> Just $ kRate `kFun` kData `kFun` kData

        NameDataTyCon DataTyConVector
         -> Just $ kData `kFun` kData

        NameDataTyCon DataTyConSegd
         -> Just $ kRate `kFun` kRate `kFun` kData

        NameDataTyCon DataTyConSel2
         -> Just $ kRate `kFun` kRate `kFun` kData

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
        -- map   :: [a b : Data]. [k : Rate]
        --       .  (a -> b) -> Stream k a -> Stream k b
        FlowOpMap 1
         -> tForalls [kData, kData, kRate]
         $  \[tA, tB, tK]
         -> (tA `tFunPE` tB)
                `tFunPE` tStream tK tA
                `tFunPE` tStream tK tB

        -- rep  :: [a : Data]. [n : Nat']
        --      .  n -> a -> Stream (Len n) a
        FlowOpRep 
         -> tForalls [kData, kNatP]
         $  \[tA, tN]
         ->     tN `tFunPE` tA `tFunPE` tStream (tLen tN) tA

        -- reps  :: [a : Data]. [k1 k2 : Rate]
        --       .  Segd   k1 k2 
        --       -> Stream k1 a
        --       -> Stream k2 a
        FlowOpReps 
         -> tForalls [kData, kRate, kRate]
         $  \[tA, tK1, tK2]
         -> tSegd tK1 tK2
                `tFunPE` tStream tK1 tA
                `tFunPE` tStream tK2 tA

        -- fold :: [a b: Data]. [k : Rate]
        --      .  (a -> b -> a) -> a -> Stream k b -> a
        FlowOpFold    
         -> tForalls [kData, kData, kRate] 
         $  \[tA, tB, tK] 
         -> (tA `tFunPE` tB `tFunPE` tA)
                `tFunPE` tA
                `tFunPE` tStream tK tA
                `tFunPE` tA

        -- folds :: [a b: Data]. [k1 k2 : Rate]
        --       .  Segd   k1 k2 
        --       -> (a -> b -> a)       -- fold operator
        --       -> Stream k1 a         -- start values
        --       -> Stream k2 b         -- source elements
        --       -> Stream k1 a         -- result values
        FlowOpFolds
         -> tForalls [kData, kData, kRate, kRate]
         $  \[tA, tB, tK1, tK2]
         -> tSegd tK1 tK2
                `tFunPE` (tA `tFunPE` tB `tFunPE` tA)
                `tFunPE` tStream tK1 tA
                `tFunPE` tStream tK2 tB
                `tFunPE` tStream tK1 tA

        -- pack  :: [a : Data]. [k1 k2 : Rate]
        --       .  Sel2 k1 k2
        --       -> Stream k1 a -> Stream k2 a
        FlowOpPack
         -> tForalls [kData, kRate, kRate]
         $  \[tA, tK1, tK2]
         -> tSel2 tK1 tK2 
                `tFunPE` tStream tK1 tA
                `tFunPE` tStream tK2 tA

        _ -> error "typeOfPrimFlow: not finished"


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

