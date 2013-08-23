
module DDC.Core.Tetra.Prim
        ( -- * Names and lexing.
          Name          (..)
        , readName

          -- * Baked-in data type constructors.
        , TyConData     (..)
        , readTyConData
        , kindTyConData

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
        , typePrimArith)
where
import DDC.Core.Tetra.Prim.Base
import DDC.Core.Tetra.Prim.TyConData
import DDC.Core.Tetra.Prim.TyConPrim
import DDC.Core.Tetra.Prim.OpStore
import DDC.Core.Tetra.Prim.OpArith
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

        NameTyConData con       -> rnf con
        NameOpStore   con       -> rnf con

        NamePrimTyCon con       -> rnf con
        NamePrimArith con       -> rnf con

        NameLitBool b           -> rnf b
        NameLitNat  n           -> rnf n
        NameLitInt  i           -> rnf i
        NameLitWord i bits      -> rnf i `seq` rnf bits


instance Pretty Name where
 ppr nn
  = case nn of
        NameVar  v              -> text v
        NameCon  c              -> text c

        NameTyConData tc        -> ppr tc
        NameOpStore op          -> ppr op

        NamePrimTyCon tc        -> ppr tc
        NamePrimArith op        -> ppr op

        NameLitBool True        -> text "True"
        NameLitBool False       -> text "False"
        NameLitNat  i           -> integer i
        NameLitInt  i           -> integer i <> text "i"
        NameLitWord i bits      -> integer i <> text "w" <> int bits


-- | Read the name of a variable, constructor or literal.
readName :: String -> Maybe Name
readName str
        -- Baked-in names.
        | Just p <- readTyConData str
        = Just $ NameTyConData p

        | Just p <- readOpStore   str
        = Just $ NameOpStore p

        -- Primitive names.
        | Just p <- readPrimTyCon str  
        = Just $ NamePrimTyCon p

        | Just p <- readPrimArith str  
        = Just $ NamePrimArith p

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
