{-# OPTIONS_HADDOCK hide #-}

module DDC.Source.Discus.Exp.Term.Prim
        ( OpFun         (..)
        , OpVector      (..)
        , OpError       (..)
        , PrimLit       (..)
        , PrimVal       (..)
        , PrimArith     (..)
        , PrimCast      (..)
        , Text)
where
import DDC.Core.Discus
        ( OpFun         (..)
        , OpVector      (..)
        , OpError       (..)
        , PrimArith     (..)
        , PrimCast      (..))

import Data.Text      (Text)


-------------------------------------------------------------------------------
-- | Primitive values.
data PrimVal
        -- | Primitive literals.
        = PrimValLit            !PrimLit

        -- | Primitive arithmetic operators.
        | PrimValArith          !PrimArith

        -- | Primitive numeric casting operators.
        | PrimValCast           !PrimCast

        -- | Primitive error handling.
        | PrimValError          !OpError

        -- | Primitive vector operators.
        | PrimValVector         !OpVector

        -- | Primitive function operators.
        | PrimValFun            !OpFun

        -- | Elaborate value.
        | PrimValElaborate

        -- | Record field projection.
        | PrimValProject        !Text

        -- | Record field shuffle.
        | PrimValShuffle

        -- | Record field combine.
        | PrimValCombine
        deriving (Eq, Ord, Show)


-------------------------------------------------------------------------------
-- | Primitive literals.
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

        -- | A character literal.
        | PrimLitChar           !Char

        -- | Text literals (UTF-8 encoded)
        | PrimLitTextLit        !Text
        deriving (Eq, Ord, Show)