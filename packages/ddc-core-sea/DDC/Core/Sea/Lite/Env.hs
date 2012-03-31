
module DDC.Core.Sea.Lite.Env
        ( primDataDefs
        , primKindEnv
        , primTypeEnv)
where
import DDC.Core.Sea.Lite.Compounds
import DDC.Core.Sea.Lite.Name
import DDC.Core.Sea.Base.Name
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
        -- Unit
        [ DataDef
                (NameDataTyCon DataTyConUnit)
                []
                (Just [ (NamePrimDaCon PrimDaConUnit, []) ])
        
        -- Int
        , DataDef
                (NameDataTyCon DataTyConInt)
                [kRegion]
                Nothing

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
                (Just   [ (NamePrimDaCon PrimDaConNil,  []) 
                        , (NamePrimDaCon PrimDaConCons, [tList (tIx kRegion 1) (tIx kData 0)])])
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
        PrimTyConPtr     -> kData `kFun` kData
        PrimTyConAddr    -> kData
        PrimTyConNat     -> kData
        PrimTyConTag     -> kData
        PrimTyConBool    -> kData
        PrimTyConWord  _ -> kData
        PrimTyConInt   _ -> kData
        PrimTyConFloat _ -> kData
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

        -- Int
        NameInt _
         -> Just $ tForall kRegion
          $ \r  -> tFun tUnit           (tAlloc r)
                                        (tBot kClosure)
                 $ tInt r

        _ -> Nothing

