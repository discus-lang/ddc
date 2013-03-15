
module DDC.Core.Flow.Name
        ( Name          (..)

        -- * Flow types and combinators.
        , FlowKiCon     (..)
        , FlowTyCon     (..)
        , FlowOp        (..)

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

        -- | Baked in data constructors.
        | NameDataCon           DataCon


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

        -- Flow constructors
        | Just p        <- readFlowKiCon str    = Just $ NameFlowKiCon p
        | Just p        <- readFlowTyCon str    = Just $ NameFlowTyCon p
        | Just p        <- readFlowOp    str    = Just $ NameFlowOp    p

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
        -- conversion
        = FlowOpToStream
        | FlowOpFromStream
        | FlowOpToVector        Int
        | FlowOpFromVector

        | FlowOpMkSel           Int

        -- maps
        | FlowOpMap             Int

        -- replicates
        | FlowOpRep
        | FlowOpReps

        -- folds
        | FlowOpFold
        | FlowOpFolds

        -- unfolds
        | FlowOpUnfold
        | FlowOpUnfolds

        -- split/combine
        | FlowOpSplit           Int
        | FlowOpCombine         Int

        -- packing
        | FlowOpPack
        deriving (Eq, Ord, Show)

instance NFData FlowOp

instance Pretty FlowOp where
 ppr pf
  = case pf of
        FlowOpToStream          -> text "toStream"              <> text "#"
        FlowOpFromStream        -> text "fromStream"            <> text "#"
        FlowOpToVector  n       -> text "toVector"   <> int n   <> text "#"
        FlowOpFromVector        -> text "fromVector"            <> text "#"

        FlowOpMkSel n           -> text "mkSel"      <> int n   <> text "#"

        FlowOpMap i             -> text "map"        <> int i   <> text "#"

        FlowOpRep               -> text "rep"                   <> text "#"
        FlowOpReps              -> text "reps"                  <> text "#"

        FlowOpFold              -> text "fold"                  <> text "#"
        FlowOpFolds             -> text "folds"                 <> text "#"

        FlowOpUnfold            -> text "unfold"                <> text "#"
        FlowOpUnfolds           -> text "unfolds"               <> text "#"

        FlowOpSplit   i         -> text "split"      <> int i   <> text "#"
        FlowOpCombine i         -> text "combine"    <> int i   <> text "#"

        FlowOpPack              -> text "pack"                  <> text "#"


-- | Read a baked-in data flow operator.
readFlowOp :: String -> Maybe FlowOp
readFlowOp str
        | Just rest     <- stripPrefix "toVector" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ FlowOpToVector arity

        | Just rest     <- stripPrefix "mkSel" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ FlowOpMkSel arity

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
                "toStream#"     -> Just $ FlowOpToStream
                "fromStream#"   -> Just $ FlowOpFromStream
                "toVector#"     -> Just $ FlowOpToVector 1
                "fromVector#"   -> Just $ FlowOpFromVector
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
        = DataTyConTuple Int    -- ^ @TN@      type constructor.
        | DataTyConArray        -- ^ @Array@   type constructor.
        | DataTyConVector       -- ^ @Vector@  type constructor.
        | DataTyConStream       -- ^ @Stream@  type constructor.
        | DataTyConSegd         -- ^ @Segd@    type constructor.
        | DataTyConSel Int      -- ^ @SelN@    type constructor.
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

