
module DDC.Source.Tetra.Prim
        ( Name          (..)
        , TyConTetra    (..)
        , kindTyConTetra

        , OpStore       (..)
        , typeOpStore

        , PrimTyCon     (..)
        , kindPrimTyCon
        , tBool
        , tNat
        , tInt
        , tWord

        , PrimArith     (..)
        , typePrimArith
        , readName)
where
import DDC.Source.Tetra.Lexer.Lit
import DDC.Source.Tetra.Prim.Base
import DDC.Source.Tetra.Prim.TyConPrim
import DDC.Source.Tetra.Prim.TyConTetra
import DDC.Source.Tetra.Prim.OpStore
import DDC.Source.Tetra.Prim.OpArith
import DDC.Base.Pretty
import Control.DeepSeq
import Data.Char

import DDC.Core.Tetra   
        ( readPrimTyCon
        , readPrimArith
        , readOpStore)


instance NFData Name where
 rnf nn
  = case nn of
        NameVar s               -> rnf s
        NameCon s               -> rnf s

        NameTyConTetra p        -> rnf p
        NameOpStore    p        -> rnf p
        NamePrimTyCon  p        -> rnf p
        NamePrimArith  p        -> rnf p

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

        NameTyConTetra p        -> ppr p
        NameOpStore   p         -> ppr p
        NamePrimTyCon p         -> ppr p
        NamePrimArith p         -> ppr p

        NameLitBool True        -> text "True"
        NameLitBool False       -> text "False"
        NameLitNat  i           -> integer i
        NameLitInt  i           -> integer i <> text "i"
        NameLitWord i bits      -> integer i <> text "w" <> int bits

        NameHole                -> text "?"


-- | Read the name of a variable, constructor or literal.
readName :: String -> Maybe Name
readName str
        -- Baked-in names
        | Just p <- readTyConTetra   str  
        = Just $ NameTyConTetra p

        | Just p <- readOpStore   str  
        = Just $ NameOpStore   p

        -- Primitive names.
        | Just p <- readPrimTyCon   str  
        = Just $ NamePrimTyCon p

        | Just p <- readPrimArith str  
        = Just $ NamePrimArith p

        -- Literal Bools
        | str == "True"  = Just $ NameLitBool True
        | str == "False" = Just $ NameLitBool False

        -- Literal Nat
        | Just val <- readLitNat str
        = Just $ NameLitNat  val

        -- Literal Ints
        | Just val <- readLitInt str
        = Just $ NameLitInt  val

        -- Literal Words
        | Just (val, bits) <- readLitWordOfBits str
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
