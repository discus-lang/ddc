
-- | Definitions of Source Tetra primitive names and operators.
module DDC.Source.Tetra.Prim
        ( -- * Primitive Types
          PrimType      (..)
        , readPrimType

          -- ** Primitive machine type constructors.
        , PrimTyCon     (..)
        , kindPrimTyCon

        , pattern KData
        , pattern KRegion
        , pattern KEffect

        , pattern TImpl
        , pattern TSusp
        , pattern TRead
        , pattern TWrite
        , pattern TAlloc

        , pattern TBool
        , pattern TNat
        , pattern TInt
        , pattern TSize
        , pattern TWord
        , pattern TFloat
        , pattern TTextLit

          -- ** Primitive tetra type constructors.
        , PrimTyConTetra(..)
        , kindPrimTyConTetra

          -- * Primitive values
        , PrimVal (..)
        , readPrimVal

          -- ** Primitive arithmetic operators.
        , PrimArith     (..)
        , typePrimArith

          -- ** Primitive casting operators.
        , PrimCast      (..)
        , typePrimCast

          -- ** Primitive vector operators.
        , OpVector      (..)
        , typeOpVector

          -- ** Primitive function operators.
        , OpFun         (..)
        , typeOpFun

          -- ** Primitive error handling
        , OpError (..)
        , typeOpError
        , makeXErrorDefault

          -- ** Primitive literals
        , PrimLit (..)
        , readPrimLit
        , primLitOfLiteral

        , pattern PTrue
        , pattern PFalse)
where
import DDC.Source.Tetra.Prim.Base
import DDC.Source.Tetra.Prim.TyCon
import DDC.Source.Tetra.Prim.TyConPrim
import DDC.Source.Tetra.Prim.TyConTetra
import DDC.Source.Tetra.Prim.OpArith
import DDC.Source.Tetra.Prim.OpCast
import DDC.Source.Tetra.Prim.OpFun
import DDC.Source.Tetra.Prim.OpVector
import DDC.Source.Tetra.Prim.OpError
import DDC.Data.Pretty
import Control.DeepSeq
import qualified Data.Text              as T

import DDC.Core.Tetra
        ( readPrimTyCon
        , readPrimArithFlag
        , readPrimCastFlag
        , readOpFun
        , readOpErrorFlag
        , readOpVectorFlag)

import DDC.Core.Salt.Name
        ( readLitNat
        , readLitInt
        , readLitSize
        , readLitWordOfBits
        , readLitFloatOfBits)


---------------------------------------------------------------------------------------------------
instance Pretty PrimType where
 ppr t
  = case t of
        PrimTypeSoCon c         -> ppr c
        PrimTypeKiCon c         -> ppr c
        PrimTypeTwCon c         -> ppr c
        PrimTypeTcCon c         -> ppr c
        PrimTypeTyCon c         -> ppr c
        PrimTypeTyConTetra c    -> ppr c


instance NFData PrimType where
 rnf t
  = case t of
        PrimTypeSoCon _         -> ()
        PrimTypeKiCon _         -> ()
        PrimTypeTwCon _         -> ()
        PrimTypeTcCon _         -> ()
        PrimTypeTyCon c         -> rnf c
        PrimTypeTyConTetra c    -> rnf c


-- | Read the name of a primitive type.
readPrimType :: String -> Maybe PrimType
readPrimType str
        | Just p <- readPrimTyConTetra str
        = Just $ PrimTypeTyConTetra p

        | Just p <- readPrimTyCon str
        = Just $ PrimTypeTyCon p

        | otherwise
        = Nothing


---------------------------------------------------------------------------------------------------
instance Pretty PrimVal where
 ppr val
  = case val of
        PrimValError    p       -> ppr p
        PrimValLit      lit     -> ppr lit
        PrimValArith    p       -> ppr p
        PrimValCast     p       -> ppr p
        PrimValVector   p       -> ppr p
        PrimValFun      p       -> ppr p


instance NFData PrimVal where
 rnf val
  = case val of
        PrimValError    p       -> rnf p
        PrimValLit      lit     -> rnf lit
        PrimValArith    p       -> rnf p
        PrimValCast     p       -> rnf p
        PrimValVector   p       -> rnf p
        PrimValFun      p       -> rnf p


-- | Read the name of a primtive value.
readPrimVal :: String -> Maybe PrimVal
readPrimVal str
        | Just (p, False) <- readOpErrorFlag str
        = Just $ PrimValError  p

        | Just lit        <- readPrimLit str
        = Just $ PrimValLit    lit

        | Just (p, False) <- readPrimArithFlag str
        = Just $ PrimValArith  p

        | Just (p, False) <- readPrimCastFlag  str
        = Just $ PrimValCast   p

        | Just (p, False) <- readOpVectorFlag  str
        = Just $ PrimValVector p

        | Just p          <- readOpFun str
        = Just $ PrimValFun    p

        | otherwise
        = Nothing


---------------------------------------------------------------------------------------------------
instance Pretty PrimLit where
 ppr lit
  = case lit of
        PrimLitBool     True    -> text "True"
        PrimLitBool     False   -> text "False"
        PrimLitNat      i       -> integer i
        PrimLitInt      i       -> integer i <> text "i"
        PrimLitSize     s       -> integer s <> text "s"
        PrimLitWord     i bits  -> integer i <> text "w" <> int bits
        PrimLitFloat    f bits  -> double  f <> text "f" <> int bits
        PrimLitChar     c       -> text (show c)
        PrimLitTextLit  tx      -> text (show $ T.unpack tx)


instance NFData PrimLit where
 rnf lit
  = case lit of
        PrimLitBool     b       -> rnf b
        PrimLitNat      n       -> rnf n
        PrimLitInt      i       -> rnf i
        PrimLitSize     s       -> rnf s
        PrimLitWord     i bits  -> rnf i `seq` rnf bits
        PrimLitFloat    d bits  -> rnf d `seq` rnf bits
        PrimLitChar     c       -> rnf c
        PrimLitTextLit  bs      -> rnf bs


-- | Read the name of a primitive literal.
readPrimLit :: String -> Maybe PrimLit
readPrimLit str
        -- Literal Bools
        | str == "True"        = Just $ PrimLitBool True
        | str == "False"       = Just $ PrimLitBool False

        -- Literal Nat
        | Just val <- readLitNat str
        = Just $ PrimLitNat  val

        -- Literal Ints
        | Just val <- readLitInt str
        = Just $ PrimLitInt  val

        -- Literal Sizes
        | Just val <- readLitSize str
        = Just $ PrimLitSize val

        -- Literal Words
        | Just (val, bits) <- readLitWordOfBits str
        , elem bits [8, 16, 32, 64]
        = Just $ PrimLitWord val bits

        -- Literal Floats
        | Just (val, mbits) <- readLitFloatOfBits str
        = case mbits of
                Just 32         -> Just $ PrimLitFloat val 32
                Just 64         -> Just $ PrimLitFloat val 64
                Nothing         -> Just $ PrimLitFloat val 64
                _               -> Nothing

        | otherwise
        = Nothing


