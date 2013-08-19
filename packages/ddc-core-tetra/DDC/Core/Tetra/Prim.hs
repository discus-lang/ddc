
module DDC.Core.Tetra.Prim
        ( -- * Names and lexing.
          Name          (..)
        , readName

          -- * Primitive type constructors.
        , TyConPrim     (..)
        , kindTyConPrim
        , readTyConPrim

          -- * Primitive arithmetic operators.
        , PrimArith   (..)
        , typePrimArith
        , readPrimArith

          -- * Mutable references.
        , PrimRef     (..)
        , typePrimRef
        , readPrimRef)
where
import DDC.Core.Tetra.Prim.Base
import DDC.Core.Tetra.Prim.TyConPrim
import DDC.Core.Tetra.Prim.PrimArith
import DDC.Core.Tetra.Prim.PrimRef
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

        NameTyConPrim con       -> rnf con
        NamePrimArith con       -> rnf con
        NamePrimRef   con       -> rnf con

        NameLitBool b           -> rnf b
        NameLitNat  n           -> rnf n
        NameLitInt  i           -> rnf i
        NameLitWord i bits      -> rnf i `seq` rnf bits


instance Pretty Name where
 ppr nn
  = case nn of
        NameVar  v              -> text v
        NameCon  c              -> text c

        NameTyConPrim tc        -> ppr tc
        NamePrimArith op        -> ppr op
        NamePrimRef   op        -> ppr op

        NameLitBool True        -> text "True"
        NameLitBool False       -> text "False"
        NameLitNat  i           -> integer i
        NameLitInt  i           -> integer i <> text "i"
        NameLitWord i bits      -> integer i <> text "w" <> int bits


-- | Read the name of a variable, constructor or literal.
readName :: String -> Maybe Name
readName str
        -- Primitive names.
        | Just p <- readTyConPrim   str  
        = Just $ NameTyConPrim p

        | Just p <- readPrimArith str  
        = Just $ NamePrimArith p

        | Just p <- readPrimRef   str  
        = Just $ NamePrimRef p

        -- Literal Bools
        | str == "True"  = Just $ NameLitBool True
        | str == "False" = Just $ NameLitBool False

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
