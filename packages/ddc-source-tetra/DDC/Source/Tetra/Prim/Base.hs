
module DDC.Source.Tetra.Prim.Base
        ( Name          (..)
        , TyConTetra    (..)
        , OpFun         (..)
        , PrimTyCon     (..)
        , PrimArith     (..))
where
import DDC.Core.Tetra    
        ( OpFun         (..)
        , PrimTyCon     (..)
        , PrimArith     (..))

import Data.ByteString  (ByteString)


-- | Names of things used in Disciple Source Tetra.
data Name
        -- | A user defined variable.
        = NameVar               String

        -- | A user defined constructor.
        | NameCon               String

        -- Baked in things ----------------------
        -- | Baked in data type constructors.
        | NameTyConTetra        TyConTetra

        -- | Baked in functional operators.
        | NameOpFun             OpFun

        -- Machine primitives -------------------
        -- | Primitive type cosntructors.
        | NamePrimTyCon         PrimTyCon

        -- | Primitive arithmetic, logic and comparison.
        | NamePrimArith         PrimArith

        -- Literals -----------------------------
        -- | A boolean literal.
        | NameLitBool           Bool

        -- | A natural literal,
        --   with enough precision to count every heap object.
        | NameLitNat            Integer

        -- | An integer literal,
        --   with enough precision to count every heap object.
        | NameLitInt            Integer

        -- | An unsigned size literal,
        --   with enough precision to count every addressable byte of memory.
        | NameLitSize           Integer

        -- | A word literal,
        --   with the given number of bits precison.
        | NameLitWord           Integer Int

        -- | A floating point literal,
        --   with the given number of bits precision.
        | NameLitFloat          Double Int

        -- | A UTF-8 string literal.
        --   Although these are represented as array literals at runtime,
        --   they have a special syntax which we want to preserve during
        --   program transformation.
        | NameLitString         ByteString

        -- Inference ----------------------------
        -- | A hole used during type inference.
        | NameHole              
        deriving (Eq, Ord, Show)


-- TyConTetra ----------------------------------------------------------------
-- | Baked-in type constructors.
data TyConTetra
        -- | @TupleN#@. Tuples.
        = TyConTetraTuple Int
        
        -- | @F#@.      Reified function values.
        | TyConTetraF

        -- | @C#@.      Reified function closures.
        | TyConTetraC

        -- | @U#@.      Explicitly unboxed values.
        | TyConTetraU

        -- | @String#@. Strings
        | TyConTetraString   
        deriving (Eq, Ord, Show)


