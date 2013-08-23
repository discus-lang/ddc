
module DDC.Source.Tetra.Prim
        ( Name          (..)
        , TyConData     (..)
        , kindTyConData

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
import DDC.Source.Tetra.Prim.TyConData
import DDC.Source.Tetra.Prim.OpStore
import DDC.Source.Tetra.Prim.OpArith
import DDC.Base.Pretty
import Data.Char


instance Pretty Name where
 ppr nn
  = case nn of
        NameVar  v              -> text v
        NameCon  c              -> text c

        NameTyConData p         -> ppr p
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
        | Just p <- readTyConData   str  
        = Just $ NameTyConData p

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
