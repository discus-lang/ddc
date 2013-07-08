
module DDC.Core.Blue.Prim
        ( -- * Names and lexing
          Name          (..)
        , readName

          -- * Primitive type constructors
        , PrimTyCon     (..)
        , kindPrimTyCon

          -- * Primitive arithmetic operators
        , PrimArith     (..)
        , typePrimArith

          -- * Casting between primitive types
        , PrimCast      (..)
        , typePrimCast)
where
import DDC.Core.Blue.Prim.Base
import DDC.Core.Blue.Prim.TyConPrim
import DDC.Core.Blue.Prim.OpPrim
import DDC.Core.Salt.Name 
        ( readPrimTyCon
        , readPrimCast
        , readPrimArith
        , readLitPrimNat
        , readLitPrimInt
        , readLitPrimWordOfBits)

import DDC.Base.Pretty
import Control.DeepSeq
import Data.Char        


instance NFData Name where
 rnf nn
  = case nn of
        NameVar s               -> rnf s
        NameCon s               -> rnf s

        NamePrimTyCon con       -> rnf con
        NamePrimArith con       -> rnf con
        NamePrimCast  c         -> rnf c

        NameLitBool b           -> rnf b
        NameLitNat  n           -> rnf n
        NameLitInt  i           -> rnf i
        NameLitWord i bits      -> rnf i `seq` rnf bits


instance Pretty Name where
 ppr nn
  = case nn of
        NameVar  v              -> text v
        NameCon  c              -> text c

        NamePrimTyCon tc        -> ppr tc
        NamePrimArith op        -> ppr op
        NamePrimCast  op        -> ppr op

        NameLitBool True        -> text "True#"
        NameLitBool False       -> text "False#"
        NameLitNat  i           -> integer i <> text "#"
        NameLitInt  i           -> integer i <> text "i" <> text "#"
        NameLitWord i bits      -> integer i <> text "w" <> int bits <> text "#"


-- | Read the name of a variable, constructor or literal.
readName :: String -> Maybe Name
readName str
        -- Primitive names.
        | Just p        <- readPrimTyCon str    = Just $ NamePrimTyCon p
        | Just p        <- readPrimArith str    = Just $ NamePrimArith p
        | Just p        <- readPrimCast  str    = Just $ NamePrimCast  p

        -- Literal Bools
        | str == "True#"  = Just $ NameLitBool True
        | str == "False#" = Just $ NameLitBool False

        -- Literal Nat
        | Just val <- readLitPrimNat str
        = Just $ NameLitNat  val

        -- Literal Ints
        | Just val <- readLitPrimInt str
        = Just $ NameLitInt  val

        -- Literal Words
        | Just (val, bits) <- readLitPrimWordOfBits str
        , elem bits [8, 16, 32, 64]
        = Just $ NameLitWord val bits

        -- Constructors.
        | c : _         <- str
        , isUpper c
        = Just $ NameCon str

        -- Variables.
        | c : _         <- str
        , isLower c      
        = Just $ NameVar str

        | otherwise
        = Nothing




