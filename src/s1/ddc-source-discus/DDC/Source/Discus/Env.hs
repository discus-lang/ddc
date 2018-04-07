
-- | Source Discus primitive type and kind environments.
module DDC.Source.Discus.Env
        ( Env           (..)

        , Presence      (..)
        , takePresent

        , empty
        , union, unions

          -- ** Type Variables
        , singletonTyVar, singletonTyVar'
        , extendTyVar
        , extendTyVar',   extendsTyVar'
        , lookupTyVar
        , tyStackDepth

          -- ** Data Constructors
        , singletonDaCon
        , extendDaCon
        , lookupDaCon

          -- * Data Defs
        , envOfDataDef

          -- ** Term Variables
        , singletonDaVar
        , singletonDaVar'
        , extendDaVar,   extendsDaVar
        , extendDaVar'
        , extendDaVarMT, extendsDaVarMT
        , lookupDaVar
        , daStackDepth

          -- * Primitive Kinds and Types.
        , kindOfPrimType
        , typeOfPrimVal
        , typeOfPrimLit)
where
import DDC.Source.Discus.Prim
import DDC.Source.Discus.Exp
import Data.Map                         (Map)
import Data.Sequence                    (Seq)
import Data.Text                        (Text)
import qualified Data.List              as List
import qualified Data.Map.Strict        as Map
import qualified Data.Sequence          as Seq


---------------------------------------------------------------------------------------------------
data Env
        = Env
        { -- | Map names of type constructors to their kinds.
          envTyCon      :: Map Text Type

          -- | Map names of variables to their kinds.
        , envTyVar      :: Map Text (Maybe Type)

          -- | Stack of kinds of deBruijn indexed type variables.
        , envTyStack    :: Seq (Maybe Type)

          -- | Map names of data constructors to their types.
        , envDaCon      :: Map Text Type

          -- | Map names of term variables to their types.
        , envDaVar      :: Map Text (Maybe Type)

          -- | Stack of types of deBruijn indexed term variables.
        , envDaStack    :: Seq (Maybe Type) }


-- | Presence of a variable in the environment.
data Presence a
        -- | Variable is not present in environment.
        = Absent

        -- | Variable is present but we don't have a type for it.
        | Unknown

        -- | Variable is present in the environment with this information.
        | Present  a
        deriving Show


-- | Yield `Just` for a `Present` and `Nothing` for the others.
takePresent :: Presence a -> Maybe a
takePresent pp
 = case pp of
        Absent          -> Nothing
        Unknown         -> Nothing
        Present x       -> Just x


-- | An empty environment.
empty :: Env
empty
        = Env
        { envTyCon      = Map.empty
        , envTyVar      = Map.empty
        , envTyStack    = Seq.empty
        , envDaCon      = Map.empty
        , envDaVar      = Map.empty
        , envDaStack    = Seq.empty }


-- | Take the right biased union of two environments.
union :: Env -> Env -> Env
union env1 env2
        = Env
        { envTyCon      = Map.union (envTyCon env1) (envTyCon env2)
        , envTyVar      = Map.union (envTyVar env1) (envTyVar env2)
        , envTyStack    = envTyStack env1 Seq.>< envTyStack env2

        , envDaCon      = Map.union (envDaCon env1) (envDaCon env2)
        , envDaVar      = Map.union (envDaVar env1) (envDaVar env2)
        , envDaStack    = envDaStack env1 Seq.>< envDaStack env2 }


-- | Take the right biased union of a list of type environments.
unions :: [Env] -> Env
unions envs
        = List.foldl' union empty envs


---------------------------------------------------------------------------------------------------
-- | Extend the environment with the kind for a type variable.
extendTyVar :: Bind -> Type -> Env -> Env
extendTyVar b k env
 = case b of
        BNone   -> env
        BAnon   -> env { envTyStack = (envTyStack env) Seq.|> (Just k) }
        BName n -> env { envTyVar   = Map.insert n (Just k) (envTyVar env) }


-- | Extend the environment with a type variable where we don't know its kind.
extendTyVar' :: Bind -> Env -> Env
extendTyVar' b env
 = case b of
        BNone   -> env
        BAnon   -> env { envTyStack = (envTyStack env) Seq.|> Nothing }
        BName n -> env { envTyVar   = Map.insert n Nothing (envTyVar env) }


-- | Extend the environment with some type variables where we don't know their kinds.
extendsTyVar' :: [Bind] -> Env -> Env
extendsTyVar' bs env
 = List.foldl' (flip extendTyVar') env bs


-- | Yield an environment containing a single type variable.
singletonTyVar' :: Bind -> Env
singletonTyVar' b
 = extendTyVar' b empty

-- | Yield an environment containing the kind for a single type variable.
singletonTyVar :: Bind -> Type -> Env
singletonTyVar b t
 = extendTyVar b t empty



-- | Lookup the kind of the given type variable.
lookupTyVar :: Env -> Bound -> Presence Type
lookupTyVar env u
 = case u of
        UName tx
         -> case Map.lookup tx (envTyVar env) of
                Nothing         -> Absent
                Just Nothing    -> Unknown
                Just (Just t)   -> Present t

        UIx i
         |  i >= Seq.length (envTyStack env)
         -> Absent

         | otherwise
         -> case Seq.index (envTyStack env) i of
                Nothing -> Unknown
                Just t  -> Present t

        UHole
         -> Unknown


-- | Get the depth of the type stack.
tyStackDepth :: Env -> Int
tyStackDepth env = Seq.length (envTyStack env)


---------------------------------------------------------------------------------------------------
-- | Extend the environment with the type of a data constructor.
extendDaCon :: DaConBind -> Type -> Env -> Env
extendDaCon (DaConBindName tx) t env
 = env { envDaCon = Map.insert tx t (envDaCon env) }


-- | Yield an environment containing the type of a single data constructor.
singletonDaCon :: DaConBind -> Type -> Env
singletonDaCon dc t
 = extendDaCon dc t empty


-- | Lookup the type of a data constructor.
lookupDaCon :: DaConBound -> Env -> Maybe Type
lookupDaCon dc env
 = case dc of
        DaConBoundName tx       -> Map.lookup tx (envDaCon env)
        DaConBoundLit  lit      -> Just (typeOfPrimLit lit)


---------------------------------------------------------------------------------------------------
-- | Extend the environment with the type of a term variable.
extendDaVar :: Bind -> Type -> Env -> Env
extendDaVar b t env
 = case b of
        BNone   -> env
        BAnon   -> env { envDaStack = (envDaStack env) Seq.|> (Just t) }
        BName n -> env { envDaVar   = Map.insert n (Just t) (envDaVar env) }


-- | Extend the environment with the types of some term variables.
extendsDaVar :: [(Bind, Type)] -> Env -> Env
extendsDaVar bxs env
 = List.foldl' (\env' (b, t) -> extendDaVar b t env') env bxs


-- | Extend the environment with a term variable where we don't know it's type.
extendDaVar' :: Bind -> Env -> Env
extendDaVar' b env
 = case b of
        BNone   -> env
        BAnon   -> env { envDaStack = (envDaStack env) Seq.|> Nothing }
        BName n -> env { envDaVar   = Map.insert n Nothing (envDaVar env) }


-- | Like `extendDaVar` but take a `BindVarMT`
extendDaVarMT :: BindVarMT -> Env -> Env
extendDaVarMT xb env
 = case xb of
        XBindVarMT b Nothing    -> extendDaVar' b env
        XBindVarMT b (Just t)   -> extendDaVar  b t env


-- | Like `extendDaVarMT` but take a list of `BindVarMT`
extendsDaVarMT :: [BindVarMT] -> Env -> Env
extendsDaVarMT bs env
 = List.foldl' (flip extendDaVarMT) env bs


-- | Yield an environment containing the type for a single term variable.
singletonDaVar :: Bind -> Type -> Env
singletonDaVar b t
 = extendDaVar b t empty


-- | Yield an environment containing a single term variable where we don't know its type.
singletonDaVar' :: Bind -> Env
singletonDaVar' b
 = extendDaVar' b empty


-- | Lookup the kind of the given type variable.
lookupDaVar :: Env -> Bound -> Presence Type
lookupDaVar env u
 = case u of
        UName tx
         -> case Map.lookup tx (envDaVar env) of
                Nothing         -> Absent
                Just Nothing    -> Unknown
                Just (Just t)   -> Present t

        UIx i
         |  i >= Seq.length (envDaStack env)
         -> Absent

         |  otherwise
         -> case Seq.index (envDaStack env) i of
                Nothing -> Unknown
                Just t  -> Present t

        UHole
         -> Unknown


-- | Get the depth of the type stack.
daStackDepth :: Env -> Int
daStackDepth env = Seq.length (envDaStack env)


---------------------------------------------------------------------------------------------------
-- | Take the kind of a primitive type.
kindOfPrimType :: PrimType -> Maybe Type
kindOfPrimType tt
 = case tt of
        PrimTypeSoCon _         -> Nothing
        PrimTypeKiCon _         -> Nothing
        PrimTypeTwCon _         -> Nothing
        PrimTypeTcCon _         -> Nothing
        PrimTypeTyCon tc        -> Just (kindPrimTyCon tc)
        PrimTypeTyConDiscus tc   -> Just (kindPrimTyConDiscus tc)


-- | Take the type of a primitive name.
typeOfPrimVal  :: PrimVal -> Type
typeOfPrimVal dc
 = case dc of
        PrimValLit      l       -> typeOfPrimLit l
        PrimValArith    p       -> typePrimArith Source p
        PrimValCast     p       -> typePrimCast  Source p
        PrimValError    p       -> typeOpError   Source p
        PrimValVector   p       -> typeOpVector  Source p
        PrimValFun      p       -> typeOpFun     Source p


-- | Take the type of a primitive literal.
typeOfPrimLit   :: PrimLit -> Type
typeOfPrimLit pl
 = case pl of
        PrimLitBool     _       -> TBool
        PrimLitNat      _       -> TNat
        PrimLitInt      _       -> TInt
        PrimLitSize     _       -> TSize
        PrimLitFloat    _ bits  -> TFloat bits
        PrimLitWord     _ bits  -> TWord  bits
        PrimLitChar     _       -> TWord  32
        PrimLitTextLit  _       -> TTextLit


---------------------------------------------------------------------------------------------------
-- | Take the types of data constructors from a data type definition.
envOfDataDef
        :: DataDef Source -> Env

envOfDataDef def
        =  unions
        $ [singletonDaCon (dataCtorName ctor) (typeOfDataCtor def ctor)
                | ctor  <- dataDefCtors def]

