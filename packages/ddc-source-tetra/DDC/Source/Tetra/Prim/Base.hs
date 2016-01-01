
-- | Definition of names used in Source Tetra language.
module DDC.Source.Tetra.Prim.Base
        ( Name          (..)
        , TyConTetra    (..)
        , OpFun         (..)
        , PrimTyCon     (..)
        , PrimArith     (..)

        , PrimVal       (..)
        , pattern NamePrimLit

        , PrimLit       (..)
        , pattern NameLitBool
        , pattern NameLitNat
        , pattern NameLitInt
        , pattern NameLitSize
        , pattern NameLitWord
        , pattern NameLitFloat
        , pattern NameLitTextLit)
where
import DDC.Core.Tetra    
        ( OpFun         (..)
        , PrimTyCon     (..)
        , PrimArith     (..))

import Data.Text        (Text)


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

        -- | Primitive Values
        | NamePrimVal           PrimVal

        -- Inference ----------------------------
        -- | A hole used during type inference.
        | NameHole              
        deriving (Eq, Ord, Show)


-- TyConTetra ----------------------------------------------------------------
-- | Baked-in type constructors.
data TyConTetra
        -- | @TupleN#@. Tuples.
        = TyConTetraTuple Int
        
        -- | @F#@.       Reified function values.
        | TyConTetraF

        -- | @C#@.       Reified function closures.
        | TyConTetraC

        -- | @U#@.       Explicitly unboxed values.
        | TyConTetraU
        deriving (Eq, Ord, Show)


-- PrimVal --------------------------------------------------------------------
-- | Primitive values.
data PrimVal
        = PrimValLit    !PrimLit
        deriving (Eq, Ord, Show)


pattern NamePrimLit lit = NamePrimVal (PrimValLit lit)


-- PrimLit --------------------------------------------------------------------
data PrimLit
        -- | A boolean literal.
        = PrimLitBool           Bool

        -- | A natural literal,
        --   with enough precision to count every heap object.
        | PrimLitNat            Integer

        -- | An integer literal,
        --   with enough precision to count every heap object.
        | PrimLitInt            Integer

        -- | An unsigned size literal,
        --   with enough precision to count every addressable byte of memory.
        | PrimLitSize           Integer

        -- | A word literal,
        --   with the given number of bits precison.
        | PrimLitWord           Integer Int

        -- | A floating point literal,
        --   with the given number of bits precision.
        | PrimLitFloat          Double Int

        -- | Text literals (UTF-8 encoded)
        | PrimLitTextLit        Text
        deriving (Eq, Ord, Show)


pattern NameLitBool   x   = NamePrimVal (PrimValLit (PrimLitBool    x))
pattern NameLitNat    x   = NamePrimVal (PrimValLit (PrimLitNat     x))
pattern NameLitInt    x   = NamePrimVal (PrimValLit (PrimLitInt     x))
pattern NameLitSize   x   = NamePrimVal (PrimValLit (PrimLitSize    x))
pattern NameLitWord   x s = NamePrimVal (PrimValLit (PrimLitWord    x s))
pattern NameLitFloat  x s = NamePrimVal (PrimValLit (PrimLitFloat   x s))
pattern NameLitTextLit x  = NamePrimVal (PrimValLit (PrimLitTextLit x))

