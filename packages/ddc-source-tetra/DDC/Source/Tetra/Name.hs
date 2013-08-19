
module DDC.Source.Tetra.Name
        ( Name          (..)
        , TyConPrim     (..)
        , PrimArith     (..)
        , PrimRef       (..)
        , readName)
where
import DDC.Source.Tetra.Lexer.Lit
import DDC.Base.Pretty
import Control.DeepSeq
import Data.Char

import DDC.Core.Tetra    
        ( TyConPrim     (..)
        , PrimArith     (..)
        , PrimRef       (..))

import DDC.Core.Tetra.Prim
        ( readTyConPrim
        , readPrimArith
        , readPrimRef)


-- | Names of things used in Disciple Source Tetra.
data Name
        -- | A user defined variable.
        = NameVar               String

        -- | A user defined constructor.
        | NameCon               String

        -- Machine primitives -------------------
        -- | Primitive type cosntructors.
        | NameTyConPrim         TyConPrim        

        -- | Primitive arithmetic, logic and comparison.
        | NamePrimArith         PrimArith

        -- | Mutable references.
        | NamePrimRef           PrimRef


        -- Literals -----------------------------
        -- | A boolean literal.
        | NameLitBool           Bool

        -- | A natural literal.
        | NameLitNat            Integer

        -- | An integer literal.
        | NameLitInt            Integer

        -- | A word literal.
        | NameLitWord           Integer Int


        -- Inference ----------------------------
        -- | A hole used during type inference.
        | NameHole              
        deriving (Eq, Ord, Show)


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

        NameHole                -> ()


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

