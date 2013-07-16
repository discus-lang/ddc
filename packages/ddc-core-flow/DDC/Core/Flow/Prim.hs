
module DDC.Core.Flow.Prim
        ( -- * Names and lexing
          Name          (..)
        , readName

          -- * Fragment specific kind constructors
        , KiConFlow     (..)
        , readKiConFlow

          -- * Fragment specific type constructors
        , TyConFlow     (..)
        , readTyConFlow
        , kindTyConFlow

          -- * Fragment specific data constructors
        , DaConFlow     (..)
        , readDaConFlow
        , typeDaConFlow

          -- * Flow operators
        , OpFlow        (..)
        , readOpFlow
        , typeOpFlow

          -- * Loop operators
        , OpLoop        (..)
        , readOpLoop
        , typeOpLoop

          -- * Store operators
        , OpStore       (..)
        , readOpStore
        , typeOpStore

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
import DDC.Core.Flow.Prim.Base
import DDC.Core.Flow.Prim.KiConFlow
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.DaConFlow
import DDC.Core.Flow.Prim.DaConPrim     ()
import DDC.Core.Flow.Prim.OpFlow
import DDC.Core.Flow.Prim.OpLoop
import DDC.Core.Flow.Prim.OpStore
import DDC.Core.Flow.Prim.OpPrim

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
        NameVar         s       -> rnf s
        NameVarMod      n s     -> rnf n `seq` rnf s
        NameCon         s       -> rnf s

        NameKiConFlow   con     -> rnf con
        NameTyConFlow   con     -> rnf con
        NameDaConFlow   con     -> rnf con
        NameOpFlow      op      -> rnf op
        NameOpLoop      op      -> rnf op
        NameOpStore     op      -> rnf op

        NamePrimTyCon   con     -> rnf con
        NamePrimArith   con     -> rnf con
        NamePrimCast    c       -> rnf c

        NameLitBool     b       -> rnf b
        NameLitNat      n       -> rnf n
        NameLitInt      i       -> rnf i
        NameLitWord     i bits  -> rnf i `seq` rnf bits


instance Pretty Name where
 ppr nn
  = case nn of
        NameVar         s       -> text s
        NameVarMod      n s     -> ppr n <> text "$" <> text s
        NameCon         c       -> text c

        NameKiConFlow   con     -> ppr con
        NameTyConFlow   con     -> ppr con
        NameDaConFlow   con     -> ppr con
        NameOpFlow      op      -> ppr op
        NameOpLoop      op      -> ppr op
        NameOpStore     op      -> ppr op

        NamePrimTyCon   tc      -> ppr tc
        NamePrimArith   op      -> ppr op
        NamePrimCast    op      -> ppr op

        NameLitBool     True    -> text "True#"
        NameLitBool     False   -> text "False#"
        NameLitNat      i       -> integer i <> text "#"
        NameLitInt      i       -> integer i <> text "i" <> text "#"
        NameLitWord     i bits  -> integer i <> text "w" <> int bits <> text "#"


-- | Read the name of a variable, constructor or literal.
readName :: String -> Maybe Name
readName str
        -- Flow fragment specific names.
        | Just p        <- readKiConFlow str    = Just $ NameKiConFlow p
        | Just p        <- readTyConFlow str    = Just $ NameTyConFlow p
        | Just p        <- readDaConFlow str    = Just $ NameDaConFlow p
        | Just p        <- readOpFlow    str    = Just $ NameOpFlow    p
        | Just p        <- readOpLoop    str    = Just $ NameOpLoop    p
        | Just p        <- readOpStore   str    = Just $ NameOpStore   p

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

        -- Variables.
        | c : _                 <- str
        , isLower c
        , Just (str1, strMod)   <- splitModString str
        , Just n                <- readName str1
        = Just $ NameVarMod n strMod

        | c : _         <- str
        , isLower c      
        = Just $ NameVar str

        -- Constructors.
        | c : _         <- str
        , isUpper c
        = Just $ NameCon str

        | otherwise
        = Nothing


-- | Strip a `...$thing` modifier from a name.
splitModString :: String -> Maybe (String, String)
splitModString str
 = case break (== '$') (reverse str) of
        (_, "")         -> Nothing
        ("", _)         -> Nothing
        (s2, _ : s1)    -> Just (reverse s1, reverse s2)

