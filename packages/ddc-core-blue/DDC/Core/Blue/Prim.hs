
module DDC.Core.Blue.Prim
        ( -- * Names and lexing.
          Name          (..)
        , readName

          -- * Primitive type constructors.
        , TyConPrim     (..)
        , kindTyConPrim

          -- * Primitive arithmetic operators.
        , OpPrimArith   (..)
        , typeOpPrimArith

          -- * Mutable references.
        , OpPrimRef     (..)
        , typeOpPrimRef)
where
import DDC.Core.Blue.Prim.Base
import DDC.Core.Blue.Prim.TyConPrim
import DDC.Core.Blue.Prim.OpPrimArith
import DDC.Core.Blue.Prim.OpPrimRef
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
        NameOpPrimArith con     -> rnf con
        NameOpPrimRef   con     -> rnf con

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
        NameOpPrimArith op      -> ppr op
        NameOpPrimRef   op      -> ppr op

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

        | Just p <- readOpPrimArith str  
        = Just $ NameOpPrimArith p

        | Just p <- readOpPrimRef   str  
        = Just $ NameOpPrimRef p

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
