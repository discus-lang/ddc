
module DDC.Core.Flow.Name
        ( Name          (..)

        -- * Flow types and combinators.
        , FlowKiCon     (..)
        , FlowTyCon     (..)
        , FlowOp        (..)

        -- * Loop combinators.
        , LoopOp        (..)

        -- * Store operators.
        , StoreOp       (..)

        -- * Data type constructors.
        , DataTyCon     (..)
        , DataCon       (..)

        -- * Primitive types and operators.
        , PrimTyCon     (..)
        , PrimArith     (..)
        , PrimCast      (..)

        -- * Name parsing
        , readName)
where
import DDC.Core.Flow.Name.Flow
import DDC.Core.Flow.Name.Proc
import DDC.Core.Salt.Name 
        ( PrimTyCon     (..),   readPrimTyCon
        , PrimCast      (..),   readPrimCast
        , PrimArith     (..),   readPrimArith
        , readLitPrimNat
        , readLitPrimInt
        , readLitPrimWordOfBits)

import DDC.Base.Pretty
import Control.DeepSeq
import Data.Typeable
import Data.List
import Data.Char        


-- | Names of things used in Disciple Core Flow.
data Name
        -- | User defined variables.
        = NameVar               String

        -- | A user defined constructor.
        | NameCon               String


        -- Flow related names -------------------
        -- | Flow kind constructors.
        | NameFlowKiCon         FlowKiCon

        -- | Flow type constructors.
        | NameFlowTyCon         FlowTyCon

        -- | Flow combinators.
        | NameFlowOp            FlowOp


        -- Proc related names -------------------
        -- | Loop combinators.
        | NameLoopOp            LoopOp

        -- | Store operators.
        | NameStoreOp           StoreOp


        -- Data types ---------------------------
        -- | Baked in data type constructors.
        | NameDataTyCon         DataTyCon

        -- | Baked in data constructors.
        | NameDataCon           DataCon


        -- Primitives ---------------------------
        -- | A primitive type constructor.
        | NamePrimTyCon         PrimTyCon

        -- | Primitive arithmetic, logic, comparison and bit-wise operators.
        | NamePrimArith         PrimArith

        -- | Primitive casting between numeric types.
        | NamePrimCast          PrimCast


        -- Literals -----------------------------
        -- | An unboxed boolean literal
        | NameLitBool           Bool

        -- | An unboxed natural literal.
        | NameLitNat            Integer

        -- | An unboxed integer literal.
        | NameLitInt            Integer

        -- | An unboxed word literal
        | NameLitWord           Integer Int
        deriving (Eq, Ord, Show, Typeable)


instance NFData Name where
 rnf nn
  = case nn of
        NameVar s               -> rnf s
        NameCon s               -> rnf s

        NameFlowKiCon con       -> rnf con
        NameFlowTyCon con       -> rnf con
        NameFlowOp    op        -> rnf op

        NameLoopOp    op        -> rnf op
        NameStoreOp   op        -> rnf op

        NameDataTyCon con       -> rnf con
        NameDataCon con         -> rnf con

        NamePrimTyCon con       -> rnf con
        NamePrimArith con       -> rnf con
        NamePrimCast  c         -> rnf c

        NameLitBool b           -> rnf b
        NameLitNat  n           -> rnf n
        NameLitInt  i           -> rnf i
        NameLitWord i bits      -> rnf i `seq` rnf bits


instance Pretty Name where
 ppr nn
  = case nn of
        NameVar  v              -> text v
        NameCon  c              -> text c

        NameFlowKiCon con       -> ppr con
        NameFlowTyCon con       -> ppr con
        NameFlowOp    op        -> ppr op

        NameLoopOp    op        -> ppr op
        NameStoreOp   op        -> ppr op

        NameDataTyCon dc        -> ppr dc
        NameDataCon   con       -> ppr con

        NamePrimTyCon tc        -> ppr tc
        NamePrimArith op        -> ppr op
        NamePrimCast  op        -> ppr op

        NameLitBool True        -> text "True#"
        NameLitBool False       -> text "False#"
        NameLitNat  i           -> integer i <> text "#"
        NameLitInt  i           -> integer i <> text "i" <> text "#"
        NameLitWord i bits      -> integer i <> text "w" <> int bits <> text "#"


-- | Read the name of a variable, constructor or literal.
readName :: String -> Maybe Name
readName str

        -- Flow names.
        | Just p        <- readFlowKiCon str    = Just $ NameFlowKiCon p
        | Just p        <- readFlowTyCon str    = Just $ NameFlowTyCon p
        | Just p        <- readFlowOp    str    = Just $ NameFlowOp    p

        -- Loop combinator names.
        | Just p        <- readLoopOp    str    = Just $ NameLoopOp    p

        -- Store operator names.
        | Just p        <- readStoreOp   str    = Just $ NameStoreOp   p

        -- Data types.
        | Just p        <- readDataTyCon str    = Just $ NameDataTyCon p
        | Just p        <- readDataCon   str    = Just $ NameDataCon   p

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


-- DataTyCon ------------------------------------------------------------------
-- | Baked-in data type constructors.
data DataTyCon
        = DataTyConTuple Int    -- ^ @TN@      type constructor.
        | DataTyConArray        -- ^ @Array@   type constructor.
        | DataTyConVector       -- ^ @Vector@  type constructor.
        | DataTyConStream       -- ^ @Stream@  type constructor.
        | DataTyConSegd         -- ^ @Segd@    type constructor.
        | DataTyConSel Int      -- ^ @SelN@    type constructor.

        | DataTyConRef          -- ^ @Ref@     type constructor.
        deriving (Eq, Ord, Show)

instance NFData DataTyCon

instance Pretty DataTyCon where
 ppr dc
  = case dc of
        DataTyConTuple n        -> text "Tuple" <> int n
        DataTyConArray          -> text "Array"
        DataTyConVector         -> text "Vector"
        DataTyConStream         -> text "Stream"
        DataTyConSegd           -> text "Segd"
        DataTyConSel n          -> text "Sel" <> int n
        DataTyConRef            -> text "Ref"


-- | Read a baked-in data type constructor.
readDataTyCon :: String -> Maybe DataTyCon
readDataTyCon str
        | Just rest     <- stripPrefix "Tuple" str
        , (ds, "")      <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ DataTyConTuple arity

        | otherwise
        = case str of
                "Array"         -> Just $ DataTyConArray
                "Vector"        -> Just $ DataTyConVector
                "Stream"        -> Just $ DataTyConStream
                "Segd"          -> Just $ DataTyConSegd
                "Sel1"          -> Just $ DataTyConSel 1
                "Sel2"          -> Just $ DataTyConSel 2
                "Ref"           -> Just $ DataTyConRef
                _               -> Nothing

-- DataCon --------------------------------------------------------------------
data DataCon
        = DataConTuple Int      -- ^ @TN@ data constructor.
        deriving (Eq, Ord, Show)

instance NFData DataCon

instance Pretty DataCon where
 ppr dc
  = case dc of
        DataConTuple n          -> text "T" <> int n


-- Read a baked in data constructor.
readDataCon :: String -> Maybe DataCon
readDataCon str
        | Just rest     <- stripPrefix "T" str
        , (ds, "")      <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ DataConTuple arity

        | otherwise
        = Nothing

