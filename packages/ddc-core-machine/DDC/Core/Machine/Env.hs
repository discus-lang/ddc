
module DDC.Core.Machine.Env
        ( primDataDefs
        , primSortEnv
        , primKindEnv
        , primTypeEnv)
where
import DDC.Core.Machine.Prim
import DDC.Type.DataDef
import DDC.Type.Exp
import DDC.Type.Exp.Simple.Compounds
import DDC.Type.Env             (Env)
import qualified DDC.Type.Env   as Env


primDataDefs :: DataDefs Name
primDataDefs
 = fromListDataDefs
        -- Hard-code maximum tuple arity to 32.
        [ makeTupleDataDef arity | arity <- [1..32] ]


-- | Make a tuple data def for the given tuple arity.
makeTupleDataDef :: Int -> DataDef Name
makeTupleDataDef n
        = makeDataDefAlg
                (NameTyConMachine (TyConTuple n))
                (replicate n (BAnon kData))
                (Just   [ ( NameDaConMachine (DaConTuple n)
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
        NameKiConMachine _              -> Just sProp
        NameTyConMachine tc             -> Just $ kindTyConMachine tc
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
        NameDaConMachine p -> Just $ typeDaConMachine p
        NameOpMachine    p -> Just $ typeOpMachine    p
        _                  -> Nothing

