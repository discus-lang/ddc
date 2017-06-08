
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
-- >  Tuple{2-32}         (T{2-32})
-- >  Vector              (none, abstract)
-- >  Series              (none, abstract)
-- 
primDataDefs :: DataDefs Name
primDataDefs
 = fromListDataDefs
 $      -- Primitive -----------------------------------------------
        -- Bool#
        [ makeDataDefAlg (NamePrimTyCon PrimTyConBool) 
                [] 
                (Just   [ (NameLitBool True,  []) 
                        , (NameLitBool False, []) ])

        -- Nat#
        , makeDataDefAlg (NamePrimTyCon PrimTyConNat)        [] Nothing

        -- Int#
        , makeDataDefAlg (NamePrimTyCon PrimTyConInt)        [] Nothing

        -- Float32#
        , makeDataDefAlg (NamePrimTyCon (PrimTyConFloat 32)) [] Nothing

        -- Float64#
        , makeDataDefAlg (NamePrimTyCon (PrimTyConFloat 64)) [] Nothing

        -- WordN#
        , makeDataDefAlg (NamePrimTyCon (PrimTyConWord 64))  [] Nothing
        , makeDataDefAlg (NamePrimTyCon (PrimTyConWord 32))  [] Nothing
        , makeDataDefAlg (NamePrimTyCon (PrimTyConWord 16))  [] Nothing
        , makeDataDefAlg (NamePrimTyCon (PrimTyConWord 8))   [] Nothing


        -- Flow -----------------------------------------------------
        -- Vector
        , makeDataDefAbs
                (NameTyConFlow TyConFlowVector)
                [BAnon kRate, BAnon kData]

        -- Series
        , makeDataDefAbs
                (NameTyConFlow TyConFlowSeries)
                [BAnon kRate, BAnon kData]
        ]

        -- Tuple
        -- Hard-code maximum tuple arity to 32.
        -- We don't have a way of avoiding the upper bound.
 ++     [ makeTupleDataDef arity
                | arity <- [2..32] ]


-- | Make a tuple data def for the given tuple arity.
makeTupleDataDef :: Int -> DataDef Name
makeTupleDataDef n
        = makeDataDefAlg
                (NameTyConFlow (TyConFlowTuple n))
                (replicate n (BAnon kData))
                (Just   [ ( NameDaConFlow (DaConFlowTuple n)
                          , (reverse [tIx kData i | i <- [0..n - 1]]))])


-- Sorts ---------------------------------------------------------------------
-- | Sort environment containing sorts of primitive kinds.
primSortEnv :: Env Name
primSortEnv  = Env.setPrimFun sortOfPrimName Env.empty


-- | Take the sort of a primitive kind name.
sortOfPrimName :: Name -> Maybe (Sort Name)
sortOfPrimName _ = Nothing


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
        NameKiConFlow _                 -> Just sProp
        NameTyConFlow tc                -> Just $ kindTyConFlow tc
        NamePrimTyCon tc                -> Just $ kindPrimTyCon tc
        _                               -> Nothing


-- Types ----------------------------------------------------------------------
-- | Type environment containing types of primitive operators.
primTypeEnv :: Env Name
primTypeEnv = Env.setPrimFun typeOfPrimName Env.empty


-- | Take the type of a name,
--   or `Nothing` if this is not a value name.
typeOfPrimName :: Name -> Maybe (Type Name)
typeOfPrimName dc
 = case dc of
        NameOpConcrete  p       -> Just $ typeOpConcrete p
        NameOpSeries    p       -> Just $ typeOpSeries   p
        NameOpStore     p       -> Just $ typeOpStore    p
        NameOpControl   p       -> Just $ typeOpControl  p
        NameOpVector    p       -> Just $ typeOpVector   p
        NameDaConFlow   p       -> Just $ typeDaConFlow  p

        NamePrimCast    p       -> Just $ typePrimCast   p 
        NamePrimArith   p       -> Just $ typePrimArith  p
        NamePrimVec     p       -> Just $ typePrimVec    p

        NameLitBool     _       -> Just $ tBool
        NameLitNat      _       -> Just $ tNat
        NameLitInt      _       -> Just $ tInt
        NameLitWord     _ bits  -> Just $ tWord bits
        NameLitFloat    _ bits  -> Just $ tFloat bits

        _                       -> Nothing

