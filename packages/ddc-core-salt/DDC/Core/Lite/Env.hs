
module DDC.Core.Lite.Env
        ( primDataDefs
        , primKindEnv
        , primTypeEnv
        , isBoxedType)
where
import DDC.Core.Lite.Compounds
import DDC.Core.Lite.Name
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
-- >  Unit                ()
-- >  Bool                B#
-- >  Nat                 N#
-- >  Int                 I#
-- >  Pair                Pr
-- >  List                Nil  Cons
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
        -- Unit
        , DataDef
                (NameDataTyCon DataTyConUnit)
                []
                (Just   [ ( NamePrimDaCon PrimDaConUnit
                          , []) ])

        -- Bool
        , DataDef
                (NameDataTyCon DataTyConBool)
                [kRegion]
                (Just   [ ( NamePrimDaCon PrimDaConBoolU
                          , [tBoolU]) ])

        -- Nat
        , DataDef
                (NameDataTyCon DataTyConNat)
                [kRegion]
                (Just   [ ( NamePrimDaCon PrimDaConNatU
                          , [tNatU]) ])
        
        -- Int
        , DataDef
                (NameDataTyCon DataTyConInt)
                [kRegion]
                (Just   [ ( NamePrimDaCon PrimDaConIntU
                          , [tIntU]) ])

        -- Pair
        , DataDef
                (NameDataTyCon DataTyConPair)
                [kRegion, kData, kData]
                (Just   [ ( NamePrimDaCon PrimDaConPr
                          , [tIx kData 1, tIx kData 0]) ])

        -- List
        , DataDef
                (NameDataTyCon DataTyConList)
                [kRegion, kData]
                (Just   [ (NamePrimDaCon PrimDaConNil,  [tUnit]) 
                        , (NamePrimDaCon PrimDaConCons, 
                                [tIx kData 0, tList (tIx kRegion 1) (tIx kData 0)])])
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
        PrimTyConVec   _ -> kData `kFun` kData


-- | Take the kind of a primitive name.
--
--   Returns `Nothing` if the name isn't primitive. 
--
kindOfPrimName :: Name -> Maybe (Kind Name)
kindOfPrimName nn
 = case nn of
        -- Console
        NameEffectTyCon EffectTyConConsole
         -> Just $ kEffect

        -- Unit
        NameDataTyCon DataTyConUnit
         -> Just $ kData

        -- Bool
        NameDataTyCon DataTyConBool
         -> Just $ kFun kRegion kData

        -- Int
        NameDataTyCon DataTyConInt
         -> Just $ kFun kRegion kData

        -- Nat
        NameDataTyCon DataTyConNat
         -> Just $ kFun kRegion kData

        -- Pair
        NameDataTyCon DataTyConPair
         -> Just $ kRegion `kFun` kData `kFun` kData `kFun` kData
        
        -- List
        NameDataTyCon DataTyConList
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
        -- B#
        NamePrimDaCon PrimDaConBoolU
         -> Just $ tForall kRegion $ \tR
                -> tFunEC tBoolU        (tAlloc tR)
                                        (tBot kClosure)
                 $ tBool tR

        -- N#
        NamePrimDaCon PrimDaConNatU
         -> Just $ tForall kRegion $ \tR
                 -> tFunEC tNatU        (tAlloc tR)
                                        (tBot kClosure)
                 $  tNat tR

        -- I#
        NamePrimDaCon PrimDaConIntU
         -> Just $ tForall kRegion $ \tR
                 -> tFunEC tIntU        (tAlloc tR)
                                        (tBot kClosure)
                 $  tInt tR

        -- Unit
        NamePrimDaCon PrimDaConUnit
         -> Just $ tUnit 

        -- Pair
        NamePrimDaCon PrimDaConPr
         -> Just $ tForalls [kRegion, kData, kData] $ \[tR, tA, tB]
                 -> tFunEC tA           (tBot kEffect)
                                        (tBot kClosure)
                 $  tFunEC tB           (tSum kEffect  [tAlloc   tR])
                                        (tSum kClosure [tDeepUse tA])
                 $  tPair tR tA tB

        -- List
        NamePrimDaCon PrimDaConNil        
         -> Just $ tForalls [kRegion, kData] $ \[tR, tA]
                -> tFunEC tUnit         (tAlloc tR)
                                        (tBot kClosure)
                 $ tList tR tA

        NamePrimDaCon PrimDaConCons
         -> Just $ tForalls [kRegion, kData] $ \[tR, tA] 
                -> tFunEC tA            (tBot kEffect)
                                        (tBot kClosure)
                 $ tFunEC (tList tR tA) (tSum kEffect  [tAlloc   tR])
                                        (tSum kClosure [tDeepUse tA])
                 $ tList tR tA

        -- Primitive arithmetic operators
        NamePrimArith p
         -> Just $ typeOfPrimArith p

        -- Unboxed Literals
        NameLitBool _      -> Just $ tBoolU
        NameLitNat  _      -> Just $ tNatU
        NameLitInt  _      -> Just $ tIntU
        NameLitWord _ bits -> Just $ tWordU bits

        _                  -> Nothing


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
        PrimArithMod    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
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


-- Unboxed --------------------------------------------------------------------
-- | Check if a type represents a boxed data type, 
--   where type variables are treated as boxed.
isBoxedType :: Type Name -> Bool
isBoxedType tt
        | TVar _        <- tt   = True
        | TForall _ t   <- tt   = isBoxedType t
        | TSum{}        <- tt   = False

        | otherwise
        = case takeTyConApps tt of
           Nothing                                              -> False
           Just (TyConSpec  TcConUnit, _)                       -> True
           Just (TyConBound (UName (NameDataTyCon _))   _, _)   -> True
           Just (TyConBound (UPrim (NameDataTyCon _) _) _,   _) -> True
           _                                                    -> False

