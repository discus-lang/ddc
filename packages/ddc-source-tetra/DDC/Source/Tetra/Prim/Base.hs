
-- | Definition of names used in Source Tetra language.
module DDC.Source.Tetra.Prim.Base
        ( -- * Names
          Name          (..)

          -- * Primitive Names
        , PrimName      (..)
        , pattern NameType
        , pattern NameVal

          -- * Primitive Types
        , PrimType      (..)
        , pattern NameTyCon
        , pattern NameTyConTetra

          -- ** Primitive machine type constructors.
        , PrimTyCon     (..)

          -- ** Primitive Tetra specific type constructors.
        , PrimTyConTetra(..)
        , pattern NameTyConTetraTuple
        , pattern NameTyConTetraVector
        , pattern NameTyConTetraF
        , pattern NameTyConTetraC
        , pattern NameTyConTetraU

          -- * Primitive Values
        , PrimVal       (..)
        , pattern NameLit
        , pattern NameArith
        , pattern NameVector
        , pattern NameFun

          -- ** Primitive arithmetic operators.
        , PrimArith     (..)

          -- ** Primitive vector operators.
        , OpVector      (..)

          -- ** Primitive function operators.
        , OpFun         (..)

          -- ** Primitive literals.
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


---------------------------------------------------------------------------------------------------
-- | Names of things used in Disciple Source Tetra.
data Name
        -- | A user defined variable.
        = NameVar               !String

        -- | A user defined constructor.
        | NameCon               !String

        -- | Primitive names.
        | NamePrim              !PrimName

        -- Inference ----------------------------
        -- | A hole used during type inference.
        | NameHole              
        deriving (Eq, Ord, Show)


---------------------------------------------------------------------------------------------------
-- | Primitive names.
data PrimName
        = PrimNameType          !PrimType
        | PrimNameVal           !PrimVal
        deriving (Eq, Ord, Show)

pattern NameType p              = NamePrim (PrimNameType p)
pattern NameVal  p              = NamePrim (PrimNameVal  p)


---------------------------------------------------------------------------------------------------
-- | Primitive types.
data PrimType
        -- | Primitive machine type constructors.
        = PrimTypeTyCon         !PrimTyCon

        -- | Primtiive type constructors specific to the Tetra fragment.
        | PrimTypeTyConTetra    !PrimTyConTetra
        deriving (Eq, Ord, Show)

pattern NameTyCon tc            = NamePrim (PrimNameType (PrimTypeTyCon tc))
pattern NameTyConTetra tc       = NamePrim (PrimNameType (PrimTypeTyConTetra tc))


---------------------------------------------------------------------------------------------------
-- | Primitive type constructors specific to the Tetra language fragment.
data PrimTyConTetra
        -- | @TupleN#@. Tuples.
        = PrimTyConTetraTuple !Int
        
        -- | @Vector#@. Vectors.
        | PrimTyConTetraVector

        -- | @F#@.       Reified function values.
        | PrimTyConTetraF

        -- | @C#@.       Reified function closures.
        | PrimTyConTetraC

        -- | @U#@.       Explicitly unboxed values.
        | PrimTyConTetraU
        deriving (Eq, Ord, Show)

pattern NameTyConTetraTuple i   = NameTyConTetra (PrimTyConTetraTuple i)
pattern NameTyConTetraVector    = NameTyConTetra PrimTyConTetraVector
pattern NameTyConTetraF         = NameTyConTetra PrimTyConTetraF
pattern NameTyConTetraC         = NameTyConTetra PrimTyConTetraC
pattern NameTyConTetraU         = NameTyConTetra PrimTyConTetraU


---------------------------------------------------------------------------------------------------
-- | Primitive values.
data PrimVal
        -- | Primitive literals.
        = PrimValLit            !PrimLit

        -- | Primitive arithmetic operators.
        | PrimValArith          !PrimArith

        -- | Primitive vector operators.
        | PrimValVector         !OpVector

        -- | Primitive function operators.
        | PrimValFun            !OpFun
        deriving (Eq, Ord, Show)

pattern NameLit    p            = NamePrim (PrimNameVal  (PrimValLit    p))
pattern NameArith  p            = NamePrim (PrimNameVal  (PrimValArith  p))
pattern NameVector p            = NamePrim (PrimNameVal  (PrimValVector p))
pattern NameFun    p            = NamePrim (PrimNameVal  (PrimValFun    p))


---------------------------------------------------------------------------------------------------
data PrimLit
        -- | A boolean literal.
        = PrimLitBool           !Bool

        -- | A natural literal,
        --   with enough precision to count every heap object.
        | PrimLitNat            !Integer

        -- | An integer literal,
        --   with enough precision to count every heap object.
        | PrimLitInt            !Integer

        -- | An unsigned size literal,
        --   with enough precision to count every addressable byte of memory.
        | PrimLitSize           !Integer

        -- | A word literal,
        --   with the given number of bits precison.
        | PrimLitWord           !Integer !Int

        -- | A floating point literal,
        --   with the given number of bits precision.
        | PrimLitFloat          !Double !Int

        -- | Text literals (UTF-8 encoded)
        | PrimLitTextLit        !Text
        deriving (Eq, Ord, Show)

pattern NameLitBool   x   = NameLit (PrimLitBool    x)
pattern NameLitNat    x   = NameLit (PrimLitNat     x)
pattern NameLitInt    x   = NameLit (PrimLitInt     x)
pattern NameLitSize   x   = NameLit (PrimLitSize    x)
pattern NameLitWord   x s = NameLit (PrimLitWord    x s)
pattern NameLitFloat  x s = NameLit (PrimLitFloat   x s)
pattern NameLitTextLit x  = NameLit (PrimLitTextLit x)


---------------------------------------------------------------------------------------------------
-- | Vector operations.
data OpVector
        -- | Allocate a new vector of the given size.
        = OpVectorAlloc

        -- | Read a value from a vector.
        | OpVectorRead

        -- | Write a value to a vector.
        | OpVectorWrite
        deriving (Eq, Ord, Show)

