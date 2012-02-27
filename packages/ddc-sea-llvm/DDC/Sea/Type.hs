-- | Sea Types.
module DDC.Sea.Type
        ( Type      (..)
        , TyCon     (..)
        , TyConPrim (..))
where
import DDC.Sea.Prim

-- | Sea types,
--   parameterised by a name type.
data Type n
        -- | An unboxed data object.
        = TCon  (TyCon n)

        -- | An unboxed pointer to something else.
        | TPtr  (Type  n)

        -- | A function taking some arguments and possibly producing a return value.
        | TFun  [Type n] (Maybe (Type n))
        deriving (Show, Eq)


-- | Type constructors,
--   parameterised by a name type.
data TyCon n
        -- | A natural number, used as a arguments to functions from the
        --   runtime tsystem. Not for literal values in the source program.
        = TyConNat

        -- | The tag of a data object.
        | TyConTag

        -- | An unboxed, primitive type.
        | TyConPrim TyConPrim

        -- | Some anonymous boxed object.
        --   This might be algabraic data, or a non-algebraic object like
        --   an array of unboxed ints, a thunk or suspension.
        | TyConObj

        -- | Some abstract type that we don't know anything about.
        --   These are types like FILE which are defined by the system libraries,
        --   and will usually be be referenced via a pointer.
        | TyConAbstract n
        deriving (Show, Eq)


