
module DDC.Core.Flow.Name
        ( Name          (..)

        -- * Primitive type constructors
        , DataTyCon     (..)
        , PrimTyCon     (..)

        -- * Primitive operators
        , PrimArith     (..)
        , PrimCast      (..)
        , PrimFlow      (..)

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

        -- | Baked in data type constructors.
        | NameDataTyCon         DataTyCon

        -- | A primitive type constructor.
        | NamePrimTyCon         PrimTyCon

        -- | Primitive arithmetic, logic, comparison and bit-wise operators.
        | NamePrimArith         PrimArith

        -- | Primitive casting between numeric types.
        | NamePrimCast          PrimCast

        -- | Primitive data flow operators.
        | NamePrimFlow          PrimFlow

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
        NameDataTyCon con       -> rnf con
        NamePrimTyCon con       -> rnf con
        NamePrimArith con       -> rnf con
        NamePrimCast  c         -> rnf c
        NamePrimFlow  f         -> rnf f
        NameLitBool b           -> rnf b
        NameLitNat  n           -> rnf n
        NameLitInt  i           -> rnf i
        NameLitWord i bits      -> rnf i `seq` rnf bits


instance Pretty Name where
 ppr nn
  = case nn of
        NameVar  v              -> text v
        NameCon  c              -> text c
        NameDataTyCon dc        -> ppr dc
        NamePrimTyCon tc        -> ppr tc
        NamePrimArith op        -> ppr op
        NamePrimFlow  op        -> ppr op
        NamePrimCast  op        -> ppr op
        NameLitBool True        -> text "True#"
        NameLitBool False       -> text "False#"
        NameLitNat  i           -> integer i <> text "#"
        NameLitInt  i           -> integer i <> text "i" <> text "#"
        NameLitWord i bits      -> integer i <> text "w" <> int bits <> text "#"


-- | Read the name of a variable, constructor or literal.
readName :: String -> Maybe Name
readName str
        |  Just name    <- readDataTyCon str
        =  Just $ NameDataTyCon name

        |  Just name    <- readPrimTyCon str
        =  Just $ NamePrimTyCon name

        -- PrimArith
        | Just p        <- readPrimArith str
        = Just $ NamePrimArith p

        -- PrimCast
        | Just p        <- readPrimCast  str
        = Just $ NamePrimCast p

        | Just p        <- readPrimFlow str
        = Just $ NamePrimFlow p

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
        = DataTyConStream       -- ^ @Stream@  type constructor.
        | DataTyConVector       -- ^ @Vector@  type constructor.
        deriving (Eq, Ord, Show)

instance NFData DataTyCon

instance Pretty DataTyCon where
 ppr dc
  = case dc of
        DataTyConStream         -> text "Stream"
        DataTyConVector         -> text "Vector"


-- | Read a baked-in data type constructor.
readDataTyCon :: String -> Maybe DataTyCon
readDataTyCon str
 = case str of
        "Stream"        -> Just DataTyConStream
        "Vector"        -> Just DataTyConVector
        _               -> Nothing


-- PrimFlow -------------------------------------------------------------------
-- | Baked-in data-flow operators.
data PrimFlow
        = PrimFlowMap      Int
        | PrimFlowFold
        | PrimFlowFolds
        | PrimFlowUnfold
        | PrimFlowUnfolds
        | PrimFlowSplit    Int
        | PrimFlowCombine  Int
        deriving (Eq, Ord, Show)

instance NFData PrimFlow

instance Pretty PrimFlow where
 ppr pf
  = case pf of
        PrimFlowMap i           -> text "map"     <> int i      <> text "#"
        PrimFlowFold            -> text "fold"                  <> text "#"
        PrimFlowFolds           -> text "folds"                 <> text "#"
        PrimFlowUnfold          -> text "unfold"                <> text "#"
        PrimFlowUnfolds         -> text "unfolds"               <> text "#"
        PrimFlowSplit   i       -> text "split"   <> int i      <> text "#"
        PrimFlowCombine i       -> text "combine" <> int i      <> text "#"


-- | Read a baked-in data flow operator.
readPrimFlow :: String -> Maybe PrimFlow
readPrimFlow str
        | Just rest     <- stripPrefix "map" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ PrimFlowMap arity

        | Just rest     <- stripPrefix "split" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ PrimFlowSplit arity

        | Just rest     <- stripPrefix "combine" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ PrimFlowCombine arity

        | otherwise
        = case str of
                "fold#"         -> Just $ PrimFlowFold
                "folds#"        -> Just $ PrimFlowFolds
                "unfold#"       -> Just $ PrimFlowUnfold
                "unfolds#"      -> Just $ PrimFlowUnfolds
                _               -> Nothing


