
module DDC.Core.Tetra.Prim
        ( -- * Names and lexing.
          Name          (..)
        , isNameHole
        , isNameLit
        , readName

          -- * Baked-in type constructors.
        , TyConTetra     (..)
        , readTyConTetra
        , kindTyConTetra
        , takeTypeOfLitName

          -- * Baked-in data constructors.
        , DaConTetra     (..)
        , readDaConTetra
        , typeDaConTetra

          -- * Baked-in store operators.
        , OpStore       (..)
        , readOpStore
        , typeOpStore

          -- * Primitive type constructors.
        , PrimTyCon     (..)
        , readPrimTyCon
        , kindPrimTyCon

          -- * Primitive arithmetic operators.
        , PrimArith     (..)
        , readPrimArith
        , typePrimArith

          -- * Primitive numeric casts.
        , PrimCast      (..)
        , readPrimCast
        , typePrimCast)
where
import DDC.Core.Tetra.Prim.Base
import DDC.Core.Tetra.Prim.TyConTetra
import DDC.Core.Tetra.Prim.TyConPrim
import DDC.Core.Tetra.Prim.DaConTetra
import DDC.Core.Tetra.Prim.OpStore
import DDC.Core.Tetra.Prim.OpArith
import DDC.Core.Tetra.Prim.OpCast
import DDC.Core.Salt.Name 
        ( readLitPrimNat
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

        NameTyConTetra con      -> rnf con
        NameDaConTetra con      -> rnf con

        NameOpStore    op       -> rnf op

        NamePrimTyCon  op       -> rnf op
        NamePrimArith  op       -> rnf op
        NamePrimCast   op       -> rnf op

        NameLitBool b           -> rnf b
        NameLitNat  n           -> rnf n
        NameLitInt  i           -> rnf i
        NameLitWord i bits      -> rnf i `seq` rnf bits

        NameHole                -> ()


instance Pretty Name where
 ppr nn
  = case nn of
        NameVar  v              -> text v
        NameCon  c              -> text c

        NameTyConTetra tc       -> ppr tc
        NameDaConTetra dc       -> ppr dc
        NameOpStore    op       -> ppr op

        NamePrimTyCon  op       -> ppr op
        NamePrimArith  op       -> ppr op
        NamePrimCast   op       -> ppr op

        NameLitBool True        -> text "True#"
        NameLitBool False       -> text "False#"
        NameLitNat  i           -> integer i <> text "#"
        NameLitInt  i           -> integer i <> text "i" <> text "#"
        NameLitWord i bits      -> integer i <> text "w" <> int bits <> text "#"

        NameHole                -> text "?"


-- | Read the name of a variable, constructor or literal.
readName :: String -> Maybe Name
readName str
        -- Baked-in names.
        | Just p <- readTyConTetra str
        = Just $ NameTyConTetra p

        | Just p <- readDaConTetra str
        = Just $ NameDaConTetra p

        | Just p <- readOpStore   str
        = Just $ NameOpStore p

        -- Primitive names.
        | Just p <- readPrimTyCon str  
        = Just $ NamePrimTyCon p

        | Just p <- readPrimArith str  
        = Just $ NamePrimArith p

        | Just p <- readPrimCast  str
        = Just $ NamePrimCast  p

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

        -- Holes
        | str == "?"
        = Just $ NameHole

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
