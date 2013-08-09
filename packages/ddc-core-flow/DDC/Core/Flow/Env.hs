
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
        -- Primitive -----------------------------------------------
 $      -- Bool#
        [ DataDef (NamePrimTyCon PrimTyConBool) 
                [] 
                (Just   [ (NameLitBool True,  []) 
                        , (NameLitBool False, []) ])

        -- Nat#
        , DataDef (NamePrimTyCon PrimTyConNat)        [] Nothing

        -- Int#
        , DataDef (NamePrimTyCon PrimTyConInt)        [] Nothing

        -- Float32#
        , DataDef (NamePrimTyCon (PrimTyConFloat 32)) [] Nothing

        -- Float64#
        , DataDef (NamePrimTyCon (PrimTyConFloat 64)) [] Nothing

        -- WordN#
        , DataDef (NamePrimTyCon (PrimTyConWord 64))  [] Nothing
        , DataDef (NamePrimTyCon (PrimTyConWord 32))  [] Nothing
        , DataDef (NamePrimTyCon (PrimTyConWord 16))  [] Nothing
        , DataDef (NamePrimTyCon (PrimTyConWord 8))   [] Nothing


        -- Flow -----------------------------------------------------
        -- Vector
        , DataDef
                (NameTyConFlow TyConFlowVector)
                [kRate, kData]
                (Just   [])

        -- Series
        , DataDef
                (NameTyConFlow TyConFlowSeries)
                [kRate, kData]
                (Just   [])
        ]

        -- Tuple
        -- Hard-code maximum tuple arity to 32.
 ++     [ makeTupleDataDef arity        | arity <- [2..32] ]


-- | Make a tuple data def for the given tuple arity.
makeTupleDataDef :: Int -> DataDef Name
makeTupleDataDef n
        = DataDef
                (NameTyConFlow (TyConFlowTuple n))
                (replicate n kData)
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
        NameKiConFlow KiConFlowRate     -> Just sProp
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
        NameOpFlow     p        -> Just $ typeOpFlow     p
        NameOpControl  p        -> Just $ typeOpControl  p
        NameOpStore    p        -> Just $ typeOpStore    p
        NameDaConFlow  p        -> Just $ typeDaConFlow  p

        NamePrimCast   p        -> Just $ typePrimCast   p 
        NamePrimArith  p        -> Just $ typePrimArith  p
        NamePrimVec    p        -> Just $ typePrimVec    p

        NameLitBool  _          -> Just $ tBool
        NameLitNat   _          -> Just $ tNat
        NameLitInt   _          -> Just $ tInt
        NameLitWord  _ bits     -> Just $ tWord bits
        NameLitFloat _ bits     -> Just $ tFloat bits

        _                       -> Nothing

