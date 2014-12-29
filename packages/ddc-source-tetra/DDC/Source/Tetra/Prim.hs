
module DDC.Source.Tetra.Prim
        ( Name          (..)

        -- * Primitive names.
        , PrimTyCon     (..)
        , kindPrimTyCon
        , tBool
        , tNat
        , tInt
        , tWord

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
import DDC.Core.Salt.Name.Lit
import DDC.Base.Pretty
import Control.DeepSeq
import Data.Char
import qualified Data.Text              as T

import DDC.Core.Tetra   
        ( pprPrimTyConStem
        , readPrimTyConStem
        , readPrimArith
        , readOpFun)


instance NFData Name where
 rnf nn
  = case nn of
        NameVar s               -> rnf s
        NameCon s               -> rnf s

        NameTyConTetra p        -> rnf p
        NameOpFun      p        -> rnf p
        NamePrimTyCon  p        -> rnf p
        NamePrimArith  p        -> rnf p

        NameLitBool    b        -> rnf b
        NameLitNat     n        -> rnf n
        NameLitInt     i        -> rnf i
        NameLitSize    s        -> rnf s
        NameLitWord    i bits   -> rnf i `seq` rnf bits
        NameLitFloat   d bits   -> rnf d `seq` rnf bits
        NameLitString  bs       -> rnf bs       

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

        NameLitBool    True     -> text "True"
        NameLitBool    False    -> text "False"
        NameLitNat     i        -> integer i
        NameLitInt     i        -> integer i <> text "i"
        NameLitSize    s        -> integer s <> text "s"
        NameLitWord    i bits   -> integer i <> text "w" <> int bits
        NameLitFloat   f bits   -> double  f <> text "f" <> int bits
        NameLitString  tx       -> text (show $ T.unpack tx)

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

        -- Literal Bools
        | str == "True"        = Just $ NameLitBool True
        | str == "False"       = Just $ NameLitBool False

        -- Literal Nat
        | Just val <- readLitNat str
        = Just $ NameLitNat  val

        -- Literal Ints
        | Just val <- readLitInt str
        = Just $ NameLitInt  val

        -- Literal Sizes
        | Just val <- readLitSize str
        = Just $ NameLitSize val

        -- Literal Words
        | Just (val, bits) <- readLitWordOfBits str
        , elem bits [8, 16, 32, 64]
        = Just $ NameLitWord val bits

        -- Literal Floats
        | Just (val, bits) <- readLitFloatOfBits str
        , elem bits [32, 64]
        = Just $ NameLitFloat val bits

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
