
module DDC.Core.Flow.Env
        ( primDataDefs
        , primSortEnv
        , primKindEnv
        , primTypeEnv)
where
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Compounds
import DDC.Type.DataDef
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
        -- Primitive -----------------------------------------------
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
                (NameTyConFlow TyConFlowStream)
                [kRate, kData]
                (Just   [])

        -- Vector
        , DataDef
                (NameTyConFlow TyConFlowVector)
                [kRate, kData]
                (Just   [])

        -- Array
        , DataDef
                (NameTyConFlow TyConFlowArray)
                [kData]
                (Just   [])

        -- Tuple
        , DataDef
                (NameTyConFlow (TyConFlowTuple 2))
                [kData, kData]
                (Just   [ ( NameDaConFlow (DaConFlowTuple 2)
                          , [tIx kData 1, tIx kData 0]) ])
        ]

-- Sorts ---------------------------------------------------------------------
primSortEnv :: Env Name
primSortEnv  = Env.setPrimFun sortOfPrimName Env.empty


-- | Take the sort of a primitive kind name.
sortOfPrimName :: Name -> Maybe (Sort Name)
sortOfPrimName _
 = Nothing


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
        NameKiConFlow KiConFlowNatP
         -> Just sProp

        NameKiConFlow KiConFlowRate
         -> Just sProp

        NameTyConFlow tc
         -> Just $ kindTyConFlow tc

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
        NameOpFlow    p         -> Just $ typeOpFlow    p
        NameOpLoop    p         -> Just $ typeOpLoop    p
        NameOpStore   p         -> Just $ typeOpStore   p
        NameDaConFlow p         -> Just $ typeDaConFlow p

        NamePrimCast p          -> Just $ typePrimCast p
        NamePrimArith p         -> Just $ typePrimArith p

        NameLitBool _           -> Just $ tBool
        NameLitNat  _           -> Just $ tNat
        NameLitInt  _           -> Just $ tInt
        NameLitWord _ bits      -> Just $ tWord bits

        _                       -> Nothing

