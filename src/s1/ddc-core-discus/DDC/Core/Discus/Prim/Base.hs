
module DDC.Core.Discus.Prim.Base
        ( Name          (..)
        , isNameHole
        , isNameLit
        , isNameLitUnboxed

        , TyConDiscus    (..)
        , DaConDiscus    (..)
        , OpError       (..)
        , OpFun         (..)
        , OpVector      (..)
        , PrimTyCon     (..)
        , PrimArith     (..)
        , PrimCast      (..))
where
import Data.Typeable
import Data.Text        (Text)
import DDC.Core.Salt.Name
        ( PrimTyCon     (..)
        , PrimArith     (..)
        , PrimCast      (..))


-- | Names of things used in Disciple Core Discus.
data Name
        -- | User defined variables.
        = NameVar               !Text

        -- | A user defined constructor.
        | NameCon               !Text

        -- | An extended name.
        | NameExt               !Name !Text

        -- | Baked-in type constructors.
        | NameTyConDiscus       !TyConDiscus

        -- | Baked-in data constructors.
        | NameDaConDiscus       !DaConDiscus

        -- | Baked-in runtime error reporting.
        --   The flag indicates whether this is the
        --   boxed (False) or unboxed (True) version.
        | NameOpError           !OpError        !Bool

        -- | Baked-in function operators.
        | NameOpFun             !OpFun

        -- | Baked-in vector operators.
        --   The flag indicates whether this is the
        --   boxed (False) or unboxed (True) version.
        | NameOpVector          !OpVector       !Bool

        -- Machine primitives ------------------
        -- | A primitive type constructor.
        | NamePrimTyCon         !PrimTyCon

        -- | Primitive arithmetic, logic, comparison and
        --   bit-wise operators.
        --   The flag indicates whether this is the
        --   boxed (False) or unboxed (True) version.
        | NamePrimArith         !PrimArith      !Bool

        -- | Primitive numeric casting operators.
        --   The flat indicates whether this is the
        --   boxed (False) or unboxed (True) version.
        | NamePrimCast          !PrimCast       !Bool

        -- Literals -----------------------------
        -- | A boolean literal.
        | NameLitBool           !Bool

        -- | A natural literal,
        --   with enough precision to count every heap object.
        | NameLitNat            !Integer

        -- | An integer literal,
        --   with enough precision to count every heap object.
        | NameLitInt            !Integer

        -- | An unsigned size literal,
        --   with enough precision to count every addressable byte of memory.
        | NameLitSize           !Integer

        -- | A word literal,
        --   with the given number of bits precision.
        | NameLitWord           !Integer !Int

        -- | A floating point literal,
        --   with the given number of bits precision.
        | NameLitFloat          !Double  !Int

        -- | A character literal,
        --   These are special syntax for a Word32 expressing a
        --   Unicode codepoint.
        | NameLitChar           !Char

        -- | A text literal (UTF-8 encoded)
        --   Note that 'Text' and 'TextLit#' are different types.
        --   The later is the primitive literal.
        | NameLitTextLit        !Text

        -- Wrappers -----------------------------
        -- | Wrapper to indicate an explicitly unboxed literal.
        | NameLitUnboxed        !Name

        -- Inference ----------------------------
        -- | Hole used during type inference.
        | NameHole
        deriving (Eq, Ord, Show, Typeable)


-- | Check whether a name is `NameHole`.
isNameHole :: Name -> Bool
isNameHole nn
 = case nn of
        NameHole         -> True
        _                -> False


-- | Check whether a name represents some literal value.
isNameLit :: Name -> Bool
isNameLit nn
 = case nn of
        NameLitBool{}    -> True
        NameLitNat{}     -> True
        NameLitInt{}     -> True
        NameLitSize{}    -> True
        NameLitWord{}    -> True
        NameLitFloat{}   -> True
        NameLitChar{}    -> True
        NameLitTextLit{} -> True
        NameLitUnboxed n -> isNameLit n
        _                -> False


-- | Check whether a name is an unboxed literal.
isNameLitUnboxed :: Name -> Bool
isNameLitUnboxed nn
 = case nn of
        NameLitUnboxed n -> isNameLit n
        _                -> False


-- TyConDiscus ----------------------------------------------------------------
-- | Baked-in type constructors.
data TyConDiscus
        -- | @TupleN#@. Tuples.
        = TyConDiscusTuple Int

        -- | @Vector#@. Vectors of unboxed values.
        | TyConDiscusVector

        -- | @U#@       Unboxed type constructor.
        --   Used to represent unboxed numeric values.
        | TyConDiscusU

        -- | @F#@       Reified function value.
        | TyConDiscusF

        -- | @C#@       Reified function closure.
        | TyConDiscusC
        deriving (Eq, Ord, Show)


-- DaConDiscus ----------------------------------------------------------------
-- | Data Constructors.
data DaConDiscus
        -- | @TN#@. Tuple data constructors.
        = DaConDiscusTuple Int
        deriving (Eq, Ord, Show)


-- OpError --------------------------------------------------------------------
-- | Operators for runtime error reporting.
data OpError
        -- | Raise an error due to inexhaustive case expressions.
        = OpErrorDefault
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
        | OpFunCApply  Int
        deriving (Eq, Ord, Show)


-- OpVector -------------------------------------------------------------------
-- | Vector operators.
data OpVector
        -- | Allocate a new vector of a given length number of elements.
        = OpVectorAlloc

        -- | Get the length of a vector, in elements.
        | OpVectorLength

        -- | Read a value from a vector.
        | OpVectorRead

        -- | Write a value to a vector.
        | OpVectorWrite
        deriving (Eq, Ord, Show)

