
module DDC.Core.Lite.Env
        ( primDataDefs
        , primKindEnv
        , primTypeEnv)
where
import DDC.Core.Lite.Compounds
import DDC.Core.Lite.Name
import DDC.Core.Salt.Name.Prim
import DDC.Type.DataDef
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Type.Env             (Env)
import qualified DDC.Type.Env   as Env


-- DataDefs -------------------------------------------------------------------
-- | Data type definitions for:
--
-- @  Type   Constructors
--  ----   ------------
--  Unit   ()
--  Int    0 1 2 3 ...
--  List   Nil Cons
-- @
primDataDefs :: DataDefs Name
primDataDefs
 = fromListDataDefs
        -- Unboxed --------------------------------------------------
        -- We need these so that we can match against unboxed patterns
        -- in case expressions.
        -- Bool#
        [ DataDef (NamePrimTyCon PrimTyConBool) 
                [] 
                (Just   [ (NameBool True,  []) 
                        , (NameBool False, []) ])

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
                        , (NamePrimDaCon PrimDaConCons, [tIx kData 0, tList (tIx kRegion 1) (tIx kData 0)])])
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
        -- Unit
        NameDataTyCon DataTyConUnit
         -> Just $ kData

        -- Bool
        NameDataTyCon DataTyConBool
         -> Just $ kFun kRegion kData

        -- Int
        NameDataTyCon DataTyConInt
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
                -> tFun tBoolU          (tAlloc tR)
                                        (tBot kClosure)
                 $ tBool tR

        -- N#
        NamePrimDaCon PrimDaConNatU
         -> Just $ tForall kRegion $ \tR
                 -> tFun tNatU          (tAlloc tR)
                                        (tBot kClosure)
                 $  tInt tR

        -- I#
        NamePrimDaCon PrimDaConIntU
         -> Just $ tForall kRegion $ \tR
                 -> tFun tIntU          (tAlloc tR)
                                        (tBot kClosure)
                 $  tInt tR

        -- Unit
        NamePrimDaCon PrimDaConUnit
         -> Just $ tUnit 

        -- Pair
        NamePrimDaCon PrimDaConPr
         -> Just $ tForalls [kRegion, kData, kData] $ \[tR, tA, tB]
                 -> tFun tA             (tBot kEffect)
                                        (tBot kClosure)
                 $  tFun tB             (tSum kEffect  [tAlloc   tR])
                                        (tSum kClosure [tDeepUse tA])
                 $  tPair tR tA tB

        -- List
        NamePrimDaCon PrimDaConNil        
         -> Just $ tForalls [kRegion, kData] $ \[tR, tA]
                -> tFun tUnit (tAlloc tR)
                              (tBot kClosure)
                 $ tList tR tA

        NamePrimDaCon PrimDaConCons
         -> Just $ tForalls [kRegion, kData] $ \[tR, tA] 
                -> tFun tA              (tBot kEffect)
                                        (tBot kClosure)
                 $ tFun (tList tR tA)   (tSum kEffect  [tAlloc   tR])
                                        (tSum kClosure [tDeepUse tA])
                 $ tList tR tA

        -- Primitive operators
        NamePrimOp p
         -> Just $ typeOfPrimOp p

        -- Unboxed Literals
        NameBool _      -> Just $ tBoolU
        NameNat  _      -> Just $ tNatU
        NameInt  _      -> Just $ tIntU
        NameWord _ bits -> Just $ tWordU bits

        _               -> Nothing


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
        PrimOpEq        -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBoolU
        PrimOpNeq       -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBoolU
        PrimOpGt        -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBoolU
        PrimOpLt        -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBoolU
        PrimOpLe        -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBoolU
        PrimOpGe        -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBoolU

        -- Boolean
        PrimOpAnd       -> tBoolU `tFunPE` tBoolU `tFunPE` tBoolU
        PrimOpOr        -> tBoolU `tFunPE` tBoolU `tFunPE` tBoolU

        -- Bitwise
        PrimOpShl       -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimOpShr       -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimOpBAnd      -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimOpBOr       -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimOpBXOr      -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t

