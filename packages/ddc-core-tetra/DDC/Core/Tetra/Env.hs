
module DDC.Core.Tetra.Env
        ( primDataDefs
        , primSortEnv
        , primKindEnv
        , primTypeEnv)
where
import DDC.Core.Tetra.Prim
import DDC.Core.Tetra.Compounds
import DDC.Type.DataDef
import DDC.Type.Exp
import DDC.Type.Env             (Env)
import qualified DDC.Type.Env   as Env


-- DataDefs -------------------------------------------------------------------
-- | Data type definitions 
--
-- >  Type                         Constructors
-- >  ----                ------------------------------
-- >  Bool                True False
-- >  Nat                 0 1 2 ...
-- >  Int                 ... -2i -1i 0i 1i 2i ...
-- >  Word{8,16,32,64}#   42w8 123w64 ...
-- 
primDataDefs :: DataDefs Name
primDataDefs
 = fromListDataDefs
        -- Primitive -----------------------------------------------
        -- Bool
        [ makeDataDef (NamePrimTyCon PrimTyConBool) 
                [] 
                (Just   [ (NameLitBool True,  []) 
                        , (NameLitBool False, []) ])

        -- Nat
        , makeDataDef (NamePrimTyCon PrimTyConNat)  [] Nothing

        -- Int
        , makeDataDef (NamePrimTyCon PrimTyConInt)  [] Nothing

        -- WordN
        , makeDataDef (NamePrimTyCon (PrimTyConWord 64)) [] Nothing
        , makeDataDef (NamePrimTyCon (PrimTyConWord 32)) [] Nothing
        , makeDataDef (NamePrimTyCon (PrimTyConWord 16)) [] Nothing
        , makeDataDef (NamePrimTyCon (PrimTyConWord 8))  [] Nothing

        -- Ref
        , makeDataDef (NameTyConData TyConDataRef) [] Nothing
        ]


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
        NameTyConData tc        -> Just $ kindTyConData tc
        NamePrimTyCon tc        -> Just $ kindPrimTyCon tc
        _                       -> Nothing


-- Types ----------------------------------------------------------------------
-- | Type environment containing types of primitive operators.
primTypeEnv :: Env Name
primTypeEnv = Env.setPrimFun typeOfPrimName Env.empty


-- | Take the type of a name,
--   or `Nothing` if this is not a value name.
typeOfPrimName :: Name -> Maybe (Type Name)
typeOfPrimName dc
 = case dc of
        NameOpStore   p         -> Just $ typeOpStore   p
        NamePrimArith p         -> Just $ typePrimArith p

        NameLitBool _           -> Just $ tBool
        NameLitNat  _           -> Just $ tNat
        NameLitInt  _           -> Just $ tInt
        NameLitWord _ bits      -> Just $ tWord bits

        _                       -> Nothing

