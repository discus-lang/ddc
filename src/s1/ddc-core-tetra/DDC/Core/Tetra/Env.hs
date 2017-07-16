
module DDC.Core.Tetra.Env
        ( primDataDefs
        , primSortEnv
        , primKindEnv
        , primTypeEnv

        , dataDefBool)
where
import DDC.Core.Tetra.Prim
import DDC.Core.Tetra.Compounds
import DDC.Type.DataDef
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
  $     [ dataDefBool

        -- Nat#
        , makeDataDefAlg (NamePrimTyCon PrimTyConNat)        [] Nothing

        -- Int#
        , makeDataDefAlg (NamePrimTyCon PrimTyConInt)        [] Nothing

        -- WordN#
        , makeDataDefAlg (NamePrimTyCon (PrimTyConWord 64))  [] Nothing
        , makeDataDefAlg (NamePrimTyCon (PrimTyConWord 32))  [] Nothing
        , makeDataDefAlg (NamePrimTyCon (PrimTyConWord 16))  [] Nothing
        , makeDataDefAlg (NamePrimTyCon (PrimTyConWord 8))   [] Nothing

        -- FloatN#
        , makeDataDefAlg (NamePrimTyCon (PrimTyConFloat 64)) [] Nothing
        , makeDataDefAlg (NamePrimTyCon (PrimTyConFloat 32)) [] Nothing

        -- TextLit#
        , makeDataDefAlg (NamePrimTyCon PrimTyConTextLit)    [] Nothing

        -- Vector#
        , makeDataDefAlg (NameTyConTetra TyConTetraVector)   [] Nothing

        -- U#
        -- We need this data def when matching against literals with case expressions.
        , makeDataDefAlg (NameTyConTetra TyConTetraU)        [] Nothing
        ]

        -- Tuple
        -- Hard-code maximum tuple arity to 32.
        -- We don't have a way of avoiding the upper bound.
 ++     [ makeTupleDataDef arity
                | arity <- [2..32] ]


-- | Data type definition for `Bool`.
dataDefBool :: DataDef Name
dataDefBool
 = makeDataDefAlg (NamePrimTyCon PrimTyConBool)
        []
        (Just   [ (NameLitBool True,  [])
                , (NameLitBool False, []) ])


-- | Make a tuple data def for the given tuple arity.
makeTupleDataDef :: Int -> DataDef Name
makeTupleDataDef n
        = makeDataDefAlg
                (NameTyConTetra (TyConTetraTuple n))
                (replicate n (BAnon kData))
                (Just   [ ( NameDaConTetra (DaConTetraTuple n)
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
        NameTyConTetra tc       -> Just $ kindTyConTetra tc
        NamePrimTyCon tc        -> Just $ kindPrimTyCon tc
        NameVar "rT"            -> Just $ kRegion
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
        NameDaConTetra  p                    -> Just $ typeDaConTetra    p
        NameOpFun       p                    -> Just $ typeOpFun         p
        NameOpVector    p f                  -> Just $ typeOpVectorFlag  p f
        NameOpError     p f                  -> Just $ typeOpErrorFlag   p f
        NamePrimArith   p f                  -> Just $ typePrimArithFlag p f
        NamePrimCast    p f                  -> Just $ typePrimCastFlag  p f

        NameLitBool     _                    -> Just $ tBool
        NameLitNat      _                    -> Just $ tNat
        NameLitInt      _                    -> Just $ tInt
        NameLitWord     _ bits               -> Just $ tWord bits
        NameLitFloat    _ bits               -> Just $ tFloat bits
        NameLitChar     _                    -> Just $ tWord 32
        NameLitTextLit  _                    -> Just $ tTextLit

        NameLitUnboxed NameLitBool{}         -> Just $ tUnboxed tBool
        NameLitUnboxed NameLitNat{}          -> Just $ tUnboxed tNat
        NameLitUnboxed NameLitInt{}          -> Just $ tUnboxed tInt
        NameLitUnboxed (NameLitWord  _ bits) -> Just $ tUnboxed (tWord bits)
        NameLitUnboxed (NameLitFloat _ bits) -> Just $ tUnboxed (tFloat bits)
        NameLitUnboxed NameLitTextLit{}      -> Just $ tUnboxed tTextLit

        _                                    -> Nothing

