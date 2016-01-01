
-- | Definitions of Source Tetra primitive names and operators.
module DDC.Source.Tetra.Prim
        ( Name          (..)

        -- * Primite type constructors.
        , PrimTyCon     (..)
        , kindPrimTyCon
        , tBool
        , tNat
        , tInt
        , tWord

        -- * Primitive values
        , PrimVal (..)
        , pattern NamePrimLit

        -- * Primitive literals
        , PrimLit (..)
        , pattern NameLitBool
        , pattern NameLitNat
        , pattern NameLitInt
        , pattern NameLitSize
        , pattern NameLitWord
        , pattern NameLitFloat
        , pattern NameLitTextLit

        -- * Arithmetic operators.
        , PrimArith     (..)
        , typePrimArith
        , readName

        -- * Tetra names.
        , TyConTetra    (..)
        , kindTyConTetra)
where
import DDC.Source.Tetra.Prim.Base
import DDC.Source.Tetra.Prim.TyConPrim
import DDC.Source.Tetra.Prim.TyConTetra
import DDC.Source.Tetra.Prim.OpArith
import DDC.Core.Lexer.Names             (isVarStart)
import DDC.Base.Pretty
import Control.DeepSeq
import Data.Char
import qualified Data.Text              as T

import DDC.Core.Tetra   
        ( pprPrimTyConStem
        , readPrimTyConStem
        , readPrimArith
        , readOpFun)

import DDC.Core.Salt.Name
        ( readLitNat
        , readLitInt
        , readLitSize
        , readLitWordOfBits
        , readLitFloatOfBits)


instance NFData Name where
 rnf nn
  = case nn of
        NameVar s               -> rnf s
        NameCon s               -> rnf s

        NameTyConTetra p        -> rnf p
        NameOpFun      p        -> rnf p
        NamePrimTyCon  p        -> rnf p
        NamePrimArith  p        -> rnf p

        NamePrimVal    v        -> rnf v

        NameHole                -> ()


instance Pretty Name where
 ppr nn
  = case nn of
        NameVar  v              -> text v
        NameCon  c              -> text c

        NameTyConTetra p        -> ppr p
        NameOpFun      p        -> ppr p
        NamePrimTyCon  p        -> pprPrimTyConStem p
        NamePrimArith  p        -> ppr p

        NamePrimVal    v        -> ppr v

        NameHole                -> text "?"


-- | Read the name of a variable, constructor or literal.
readName :: String -> Maybe Name
readName str
        -- Baked-in names
        | Just p <- readTyConTetra   str  
        = Just $ NameTyConTetra p

        | Just p <- readOpFun     str
        = Just $ NameOpFun     p

        -- Primitive names.
        | Just p <- readPrimTyConStem   str  
        = Just $ NamePrimTyCon p

        | Just p <- readPrimArith str  
        = Just $ NamePrimArith p

        | Just v <- readPrimVal str
        = Just $ NamePrimVal   v

        -- Constructors.
        | c : _         <- str
        , isUpper c
        = Just $ NameCon str

        -- Variables.
        | c : _         <- str
        , isVarStart c      
        = Just $ NameVar str

        | otherwise
        = Nothing


---------------------------------------------------------------------------------------------------
instance Pretty PrimVal where
 ppr val
  = case val of
        PrimValLit lit          -> ppr lit


instance NFData PrimVal where
 rnf val
  = case val of
        PrimValLit lit          -> rnf lit


-- | Read the name of a primtive value.
readPrimVal :: String -> Maybe PrimVal
readPrimVal str
        | Just lit      <- readPrimLit str
        = Just $ PrimValLit lit

        | otherwise
        = Nothing


---------------------------------------------------------------------------------------------------
instance Pretty PrimLit where
 ppr lit
  = case lit of
        PrimLitBool    True     -> text "True"
        PrimLitBool    False    -> text "False"
        PrimLitNat     i        -> integer i
        PrimLitInt     i        -> integer i <> text "i"
        PrimLitSize    s        -> integer s <> text "s"
        PrimLitWord    i bits   -> integer i <> text "w" <> int bits
        PrimLitFloat   f bits   -> double  f <> text "f" <> int bits
        PrimLitTextLit tx       -> text (show $ T.unpack tx)


instance NFData PrimLit where
 rnf lit 
  = case lit of
        PrimLitBool    b        -> rnf b
        PrimLitNat     n        -> rnf n
        PrimLitInt     i        -> rnf i
        PrimLitSize    s        -> rnf s
        PrimLitWord    i bits   -> rnf i `seq` rnf bits
        PrimLitFloat   d bits   -> rnf d `seq` rnf bits
        PrimLitTextLit bs       -> rnf bs       


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
        | Just (val, bits) <- readLitFloatOfBits str
        , elem bits [32, 64]
        = Just $ PrimLitFloat val bits

        | otherwise
        = Nothing

