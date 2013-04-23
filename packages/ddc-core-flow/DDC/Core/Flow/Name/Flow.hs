
-- | Flow related names.
module DDC.Core.Flow.Name.Flow
        ( FlowKiCon (..),       readFlowKiCon
        , FlowTyCon (..),       readFlowTyCon
        , FlowOp    (..),       readFlowOp)
where
import DDC.Base.Pretty
import Control.DeepSeq
import Data.List
import Data.Char        


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
        | FlowOpLengthOfStream

        | FlowOpToVector        Int
        | FlowOpFromVector

        -- rate conversion
        | FlowOpLengthOfRate    

        -- selectors
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
        FlowOpLengthOfStream    -> text "lengthOfStream"        <> text "#"

        FlowOpToVector  n       -> text "toVector"   <> int n   <> text "#"
        FlowOpFromVector        -> text "fromVector"            <> text "#"

        FlowOpLengthOfRate      -> text "lengthOfRate"          <> text "#"

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
                "toStream#"       -> Just $ FlowOpToStream
                "fromStream#"     -> Just $ FlowOpFromStream
                "lengthOfStream#" -> Just $ FlowOpLengthOfStream

                "toVector#"       -> Just $ FlowOpToVector 1
                "fromVector#"     -> Just $ FlowOpFromVector

                "lengthOfRate#"   -> Just $ FlowOpLengthOfRate

                "map#"            -> Just $ FlowOpMap 1
                "rep#"            -> Just $ FlowOpRep
                "reps#"           -> Just $ FlowOpReps
                "fold#"           -> Just $ FlowOpFold
                "folds#"          -> Just $ FlowOpFolds
                "unfold#"         -> Just $ FlowOpUnfold
                "unfolds#"        -> Just $ FlowOpUnfolds
                "pack#"           -> Just $ FlowOpPack
                _                 -> Nothing

