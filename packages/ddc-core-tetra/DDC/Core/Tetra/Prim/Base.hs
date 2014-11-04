
module DDC.Core.Tetra.Prim.Base
        ( Name          (..)
        , isNameHole
        , isNameLit
        
        , TyConTetra    (..)
        , DaConTetra    (..)
        , OpStore       (..)
        , OpFun         (..)
        , PrimTyCon     (..)
        , PrimArith     (..)
        , PrimCast      (..))
where
import Data.Typeable
import DDC.Core.Salt.Name
        ( PrimTyCon     (..)
        , PrimArith     (..)
        , PrimCast      (..))


-- | Names of things used in Disciple Core Tetra.
data Name
        -- | User defined variables.
        = NameVar               String

        -- | A user defined constructor.
        | NameCon               String

        -- | An extended name.
        | NameExt               Name String

        -- | Baked-in type constructors.
        | NameTyConTetra        TyConTetra

        -- | Baked-in data constructors.
        | NameDaConTetra        DaConTetra

        -- | Baked-in function operators.
        | NameOpFun             OpFun

        -- | Baked-in store operators.
        | NameOpStore           OpStore

        -- Machine primitives ------------------
        -- | A primitive type constructor.
        | NamePrimTyCon         PrimTyCon

        -- | Primitive arithmetic, logic, comparison and bit-wise operators.
        | NamePrimArith         PrimArith

        -- | Primitive numeric casting operators.
        | NamePrimCast          PrimCast

        -- Literals -----------------------------
        -- | A boolean literal.
        | NameLitBool           Bool

        -- | A natural literal.
        | NameLitNat            Integer

        -- | An integer literal.
        | NameLitInt            Integer

        -- | A word literal.
        | NameLitWord           Integer Int

        -- Inference ----------------------------
        -- | Hole used during type inference.
        | NameHole 
        deriving (Eq, Ord, Show, Typeable)


-- | Check whether a name is `NameHole`.
isNameHole :: Name -> Bool
isNameHole nn
 = case nn of
        NameHole        -> True
        _               -> False


-- | Check whether a name represents some literal value.
isNameLit :: Name -> Bool
isNameLit nn
 = case nn of
        NameLitBool{}   -> True
        NameLitNat{}    -> True
        NameLitInt{}    -> True
        NameLitWord{}   -> True
        _               -> False


-- TyConTetra ----------------------------------------------------------------
-- | Baked-in type constructors.
data TyConTetra
        -- | @Ref#@.    Mutable reference.
        = TyConTetraRef

        -- | @TupleN#@. Tuples.
        | TyConTetraTuple Int

        -- | @B#@.      Boxing type constructor. 
        --   Used to represent boxed numeric values.
        | TyConTetraB

        -- | @U#@.      Unboxed type constructor.
        --   Used to represent unboxed numeric values.
        | TyConTetraU

        -- | @F#@.      Reified function value.
        | TyConTetraF

        -- | @C#@.      Reified function closure.
        | TyConTetraC
        deriving (Eq, Ord, Show)


-- DaConTetra ----------------------------------------------------------------
-- | Data Constructors.
data DaConTetra
        -- | @TN#@. Tuple data constructors.
        = DaConTetraTuple Int
        deriving (Eq, Ord, Show)


-- OpStore -------------------------------------------------------------------
-- | Mutable References.
data OpStore
        = OpStoreAllocRef     -- ^ Allocate a reference.
        | OpStoreReadRef      -- ^ Read a reference.
        | OpStoreWriteRef     -- ^ Write to a reference.
        | OpStoreAllocPtr     -- ^ Allocate a pointer.
        | OpStoreReadPtr      -- ^ Read an offset pointer.
        | OpStoreWritePtr     -- ^ Write to an offset pointer.
        | OpStoreProj Int Int -- ^ A projection out of a tuple: size of tuple, then desired index
        deriving (Eq, Ord, Show)


-- OpFun ----------------------------------------------------------------------
-- | Operators for building function values and closures.
--   The implicit versions work on functions of type (a -> b), 
--   while the explicit versions use expliciy closure types like C# (a -> b).
data OpFun
        -- | Partially apply a supecombinator to some arguments, producing
        --   an implicitly typed closure.
        = OpFunCurry   Int

        -- | Apply an implicitly typed closure to some more arguments.
        | OpFunApply   Int

        -- | Reify a function into an explicit functional value.
        | OpFunCReify

        -- | Apply an explicit functional value to some arguments,
        --   producing an explicitly typed closure.
        | OpFunCCurry  Int

        -- | Extend an explicitly typed closure with more arguments,
        --   producing a new closure.
        | OpFunCExtend Int

        -- | Apply an explicitly typed closure to some arguments,
        --   possibly evaluating the contained function.
        | OpFunCApply   Int
        deriving (Eq, Ord, Show)

