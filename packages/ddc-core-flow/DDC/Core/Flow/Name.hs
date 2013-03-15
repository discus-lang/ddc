
module DDC.Core.Flow.Name
        ( Name          (..)

        -- * Flow types and combinators.
        , FlowKiCon     (..)
        , FlowTyCon     (..)
        , FlowOp        (..)

        -- * Data type constructors.
        , DataTyCon     (..)

        -- * Primitive types and operators.
        , PrimTyCon     (..)
        , PrimArith     (..)
        , PrimCast      (..)

        -- * Name parsing
        , readName)
where
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

        -- | Flow rate kind.
        | NameFlowKiCon         FlowKiCon

        -- | Flow type constructors.
        | NameFlowTyCon         FlowTyCon

        -- | Flow combinators.
        | NameFlowOp            FlowOp

        -- | Baked in data type constructors.
        | NameDataTyCon         DataTyCon

        -- | A primitive type constructor.
        | NamePrimTyCon         PrimTyCon

        -- | Primitive arithmetic, logic, comparison and bit-wise operators.
        | NamePrimArith         PrimArith

        -- | Primitive casting between numeric types.
        | NamePrimCast          PrimCast

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

        NameDataTyCon con       -> rnf con

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

        NameDataTyCon dc        -> ppr dc

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

        -- Flow constructors
        | Just p        <- readFlowKiCon str    = Just $ NameFlowKiCon p
        | Just p        <- readFlowTyCon str    = Just $ NameFlowTyCon p
        | Just p        <- readFlowOp    str    = Just $ NameFlowOp    p

        -- Data type constructors.
        | Just p        <- readDataTyCon str    = Just $ NameDataTyCon p

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


-- FlowKiCon ------------------------------------------------------------------
-- | Flow kind constructors.
data FlowKiCon
        = FlowKiConNatP
        | FlowKiConRate
        deriving (Eq, Ord, Show)

instance NFData FlowKiCon

instance Pretty FlowKiCon where
 ppr con
  = case con of
        FlowKiConNatP   -> text "Nat'"
        FlowKiConRate   -> text "Rate"


-- | Read a flow kind constructor.
readFlowKiCon :: String -> Maybe FlowKiCon
readFlowKiCon str
 = case str of
        "Nat'"  -> Just $ FlowKiConNatP
        "Rate"  -> Just $ FlowKiConRate
        _       -> Nothing


-- FlowTyCon ------------------------------------------------------------------
data FlowTyCon
        = FlowTyConNatP  Int
        | FlowTyConLen
        deriving (Eq, Ord, Show)

instance NFData FlowTyCon

instance Pretty FlowTyCon where
 ppr con
  = case con of
        FlowTyConNatP n -> int n <> text "'"
        FlowTyConLen    -> text "Len"


-- | Read a flow type constructor.
readFlowTyCon :: String -> Maybe FlowTyCon
readFlowTyCon str
        | (ds, str2)    <- span isDigit str
        , not $ null ds
        , Just ""       <- stripPrefix "'" str2
        = Just $ FlowTyConNatP (read ds)

        | otherwise
        = case str of
                "Len"   -> Just $ FlowTyConLen
                _       -> Nothing


-- FlowOp ---------------------------------------------------------------------
-- | Flow combinators.
data FlowOp
        = FlowOpMap      Int
        | FlowOpRep
        | FlowOpReps
        | FlowOpFold
        | FlowOpFolds
        | FlowOpUnfold
        | FlowOpUnfolds
        | FlowOpSplit    Int
        | FlowOpCombine  Int
        | FlowOpPack
        deriving (Eq, Ord, Show)

instance NFData FlowOp

instance Pretty FlowOp where
 ppr pf
  = case pf of
        FlowOpMap i     -> text "map"     <> int i      <> text "#"
        FlowOpRep       -> text "rep"                   <> text "#"
        FlowOpReps      -> text "reps"                  <> text "#"
        FlowOpFold      -> text "fold"                  <> text "#"
        FlowOpFolds     -> text "folds"                 <> text "#"
        FlowOpUnfold    -> text "unfold"                <> text "#"
        FlowOpUnfolds   -> text "unfolds"               <> text "#"
        FlowOpSplit   i -> text "split"   <> int i      <> text "#"
        FlowOpCombine i -> text "combine" <> int i      <> text "#"
        FlowOpPack      -> text "pack"                  <> text "#"

-- | Read a baked-in data flow operator.
readFlowOp :: String -> Maybe FlowOp
readFlowOp str
        | Just rest     <- stripPrefix "map" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ FlowOpMap arity

        | Just rest     <- stripPrefix "split" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ FlowOpSplit arity

        | Just rest     <- stripPrefix "combine" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ FlowOpCombine arity

        | otherwise
        = case str of
                "map#"          -> Just $ FlowOpMap 1
                "rep#"          -> Just $ FlowOpRep
                "reps#"         -> Just $ FlowOpReps
                "fold#"         -> Just $ FlowOpFold
                "folds#"        -> Just $ FlowOpFolds
                "unfold#"       -> Just $ FlowOpUnfold
                "unfolds#"      -> Just $ FlowOpUnfolds
                "pack#"         -> Just $ FlowOpPack
                _               -> Nothing


-- DataTyCon ------------------------------------------------------------------
-- | Baked-in data type constructors.
data DataTyCon
        = DataTyConStream       -- ^ @Stream@  type constructor.
        | DataTyConVector       -- ^ @Vector@  type constructor.
        | DataTyConSegd         -- ^ @Segd@    type constructor.
        | DataTyConSel2         -- ^ @Sel2@     type constructor.
        deriving (Eq, Ord, Show)

instance NFData DataTyCon

instance Pretty DataTyCon where
 ppr dc
  = case dc of
        DataTyConStream -> text "Stream"
        DataTyConVector -> text "Vector"
        DataTyConSegd   -> text "Segd"
        DataTyConSel2   -> text "Sel2"


-- | Read a baked-in data type constructor.
readDataTyCon :: String -> Maybe DataTyCon
readDataTyCon str
 = case str of
        "Stream"        -> Just DataTyConStream
        "Vector"        -> Just DataTyConVector
        "Segd"          -> Just DataTyConSegd
        "Sel2"          -> Just DataTyConSel2
        _               -> Nothing

