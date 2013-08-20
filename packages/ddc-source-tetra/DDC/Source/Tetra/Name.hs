
module DDC.Source.Tetra.Name
        ( Name          (..)
        , TyConPrim     (..)
        , PrimArith     (..)
        , PrimRef       (..)
        , readName)
where
import DDC.Source.Tetra.Lexer.Lit
import DDC.Source.Tetra.Name.Base
import DDC.Base.Pretty
import Data.Char

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

        NameHole                -> text "?"


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
