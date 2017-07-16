
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

          -- * Fusable Flow operators
        , OpConcrete    (..)
        , readOpConcrete
        , typeOpConcrete

          -- * Series operators
        , OpSeries      (..)
        , readOpSeries
        , typeOpSeries

          -- * Control operators
        , OpControl     (..)
        , readOpControl
        , typeOpControl

          -- * Store operators
        , OpStore       (..)
        , readOpStore
        , typeOpStore

          -- * Store operators
        , OpVector      (..)
        , readOpVector
        , typeOpVector

          -- * Primitive type constructors
        , PrimTyCon     (..)
        , kindPrimTyCon

          -- * Primitive arithmetic operators
        , PrimArith     (..)
        , typePrimArith

          -- * Primitive vector operators
        , PrimVec    (..)
        , typePrimVec
        , multiOfPrimVec
        , liftPrimArithToVec
        , lowerPrimVecToArith

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
import DDC.Core.Flow.Prim.OpConcrete
import DDC.Core.Flow.Prim.OpControl
import DDC.Core.Flow.Prim.OpSeries
import DDC.Core.Flow.Prim.OpStore
import DDC.Core.Flow.Prim.OpVector
import DDC.Core.Flow.Prim.OpPrim

import DDC.Core.Lexer.Tokens            (isVarStart)

import DDC.Core.Salt.Name
        ( readPrimTyCon

        , readPrimArith

        , readPrimVec
        , multiOfPrimVec
        , liftPrimArithToVec
        , lowerPrimVecToArith

        , readPrimCast
        , readLitNat
        , readLitInt
        , readLitWordOfBits
        , readLitFloatOfBits)

import DDC.Data.Name
import DDC.Data.Pretty
import DDC.Data.ListUtils
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
        NameOpConcrete  op      -> rnf op
        NameOpControl   op      -> rnf op
        NameOpSeries    op      -> rnf op
        NameOpStore     op      -> rnf op
        NameOpVector    op      -> rnf op

        NamePrimTyCon   op      -> rnf op
        NamePrimArith   op      -> rnf op
        NamePrimVec     op      -> rnf op
        NamePrimCast    op      -> rnf op

        NameLitBool     b       -> rnf b
        NameLitNat      n       -> rnf n
        NameLitInt      i       -> rnf i
        NameLitWord     i bits  -> rnf i `seq` rnf bits
        NameLitFloat    r bits  -> rnf r `seq` rnf bits


instance Pretty Name where
 ppr nn
  = case nn of
        NameVar         s       -> text s
        NameVarMod      n s     -> ppr n <> text "$" <> text s
        NameCon         c       -> text c

        NameKiConFlow   con     -> ppr con
        NameTyConFlow   con     -> ppr con
        NameDaConFlow   con     -> ppr con
        NameOpConcrete  op      -> ppr op
        NameOpControl   op      -> ppr op
        NameOpSeries    op      -> ppr op
        NameOpStore     op      -> ppr op
        NameOpVector    op      -> ppr op

        NamePrimTyCon   tc      -> ppr tc
        NamePrimArith   op      -> ppr op
        NamePrimVec     op      -> ppr op
        NamePrimCast    op      -> ppr op

        NameLitBool     True    -> text "True#"
        NameLitBool     False   -> text "False#"
        NameLitNat      i       -> integer  i <> text "#"
        NameLitInt      i       -> integer  i <> text "i" <> text "#"
        NameLitWord     i bits  -> integer  i <> text "w" <> int bits <> text "#"
        NameLitFloat    r bits  -> double (fromRational r) <> text "f" <> int bits <> text "#"


instance CompoundName Name where
 extendName n str
  = NameVarMod n str

 splitName nn
  = case nn of
        NameVarMod n str   -> Just (n, str)
        _                  -> Nothing


-- | Read the name of a variable, constructor or literal.
readName :: String -> Maybe Name
readName str
        -- Flow fragment specific names.
        | Just p        <- readKiConFlow  str   = Just $ NameKiConFlow  p
        | Just p        <- readTyConFlow  str   = Just $ NameTyConFlow  p
        | Just p        <- readDaConFlow  str   = Just $ NameDaConFlow  p
        | Just p        <- readOpConcrete str   = Just $ NameOpConcrete p
        | Just p        <- readOpControl  str   = Just $ NameOpControl  p
        | Just p        <- readOpSeries   str   = Just $ NameOpSeries   p
        | Just p        <- readOpStore    str   = Just $ NameOpStore    p
        | Just p        <- readOpVector   str   = Just $ NameOpVector   p

        -- Primitive names.
        | Just p        <- readPrimTyCon  str   = Just $ NamePrimTyCon  p
        | Just p        <- readPrimArith  str   = Just $ NamePrimArith  p
        | Just p        <- readPrimVec    str   = Just $ NamePrimVec    p
        | Just p        <- readPrimCast   str   = Just $ NamePrimCast   p

        -- Literal Bools
        | str == "True#"  = Just $ NameLitBool True
        | str == "False#" = Just $ NameLitBool False

        -- Literal Nat
        | Just str'     <- stripSuffix "#" str
        , Just val      <- readLitNat str'
        = Just $ NameLitNat  val

        -- Literal Ints
        | Just str'     <- stripSuffix "#" str
        , Just val      <- readLitInt str'
        = Just $ NameLitInt  val

        -- Literal Words
        | Just str'             <- stripSuffix "#" str
        , Just (val, bits)      <- readLitWordOfBits str'
        , elem bits [8, 16, 32, 64]
        = Just $ NameLitWord val bits

        -- Literal Floats
        | Just str'             <- stripSuffix "#" str
        , Just (val, mBits)     <- readLitFloatOfBits str'
        = case mBits of
                Just 32         -> Just $ NameLitFloat (toRational val) 32
                Just 64         -> Just $ NameLitFloat (toRational val) 64
                _               -> Nothing

        -- Variables.
        | c : _                 <- str
        , isVarStart c
        , Just (str1, strMod)   <- splitModString str
        , Just n                <- readName str1
        = Just $ NameVarMod n strMod

        | c : _         <- str
        , isVarStart c
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

