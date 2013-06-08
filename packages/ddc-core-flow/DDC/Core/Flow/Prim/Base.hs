
module DDC.Core.Flow.Prim.Base
        ( Name (..)
        , KiConFlow     (..)
        , TyConFlow     (..)
        , DaConFlow     (..)
        , OpFlow        (..)
        , OpLoop        (..)
        , OpStore       (..)
        , PrimTyCon     (..)
        , PrimArith     (..)
        , PrimCast      (..))
where
import Data.Typeable
import DDC.Core.Salt.Name 
        ( PrimTyCon     (..)
        , PrimArith     (..)
        , PrimCast      (..))

-- | Names of things used in Disciple Core Flow.
data Name
        -- | User defined variables.
        = NameVar               String

        -- | A user defined constructor.
        | NameCon               String

        -- Fragment specific names -----------
        -- | Fragment specific kind constructors.
        | NameKiConFlow         KiConFlow

        -- | Fragment specific type constructors.
        | NameTyConFlow         TyConFlow

        -- | Fragment specific data constructors.
        | NameDaConFlow         DaConFlow

        -- | Flow operators.
        | NameOpFlow            OpFlow

        -- | Loop operators.
        | NameOpLoop            OpLoop

        -- | Store operators.
        | NameOpStore           OpStore


        -- Machine primitives ------------------
        -- | A primitive type constructor.
        | NamePrimTyCon         PrimTyCon

        -- | Primitive arithmetic, logic, comparison and bit-wise operators.
        | NamePrimArith         PrimArith

        -- | Primitive casting between numeric types.
        | NamePrimCast          PrimCast


        -- Literals -----------------------------
        -- | A boolean literal.
        | NameLitBool           Bool

        -- | A natural literal.
        | NameLitNat            Integer

        -- | An integer literal.
        | NameLitInt            Integer

        -- | A word literal.
        | NameLitWord           Integer Int
        deriving (Eq, Ord, Show, Typeable)


-- | Fragment specific kind constructors.
data KiConFlow
        = KiConFlowRate
        deriving (Eq, Ord, Show)


-- | Fragment specific type constructors.
data TyConFlow
        -- | @TupleN#@ constructor. Tuples.
        = TyConFlowTuple Int            

        -- | @Vector#@ constructor. Vectors. 
        | TyConFlowVector

        -- | @Series#@ constructor. Series types.
        | TyConFlowSeries

        -- | @Segd#@   constructor. Segment Descriptors.
        | TyConFlowSegd

        -- | @SelN#@   constructor. Selectors.
        | TyConFlowSel Int

        -- | @Ref#@    constructor. References.
        | TyConFlowRef                  

        -- | @World#@  constructor. State token used when converting to GHC core.
        | TyConFlowWorld

        -- | @RateNat#@ constructor. Naturals witnessing a type-level Rate.          
        | TyConFlowRateNat
        deriving (Eq, Ord, Show)


-- | Primitive data constructors.
data DaConFlow
        = DaConFlowTuple Int            -- ^ @TN@ data constructor.
        deriving (Eq, Ord, Show)


-- | Flow operators.
data OpFlow
        -- Stream conversions.
        = OpFlowVectorOfSeries
        | OpFlowRateOfSeries

        -- selectors
        | OpFlowMkSel Int

        -- maps
        | OpFlowMap Int

        -- replicates
        | OpFlowRep
        | OpFlowReps

        -- folds
        | OpFlowFold
        | OpFlowFoldIndex
        | OpFlowFolds

        -- unfolds
        | OpFlowUnfold
        | OpFlowUnfolds

        -- split/combine
        | OpFlowSplit   Int
        | OpFlowCombine Int

        -- packing
        | OpFlowPack
        deriving (Eq, Ord, Show)


-- | Loop operators.
data OpLoop
        = OpLoopLoop
        | OpLoopLoopN
        | OpLoopGuard
        deriving (Eq, Ord, Show)


-- | Store operators.
data OpStore
        -- Assignables.
        = OpStoreNew            -- ^ Allocate a new reference.
        | OpStoreRead           -- ^ Read from a reference.
        | OpStoreWrite          -- ^ Write to a reference.

        -- Vectors.
        | OpStoreNewVector      -- ^ Allocate a new vector (taking a @Nat@ for the length)
        | OpStoreNewVectorR     -- ^ Allocate a new vector (taking a @Rate@ for the length)
        | OpStoreNewVectorN     -- ^ Allocate a new vector (taking a @RateNat@ for the length)
        | OpStoreReadVector     -- ^ Read from a vector.
        | OpStoreWriteVector    -- ^ Write to a vector.
        | OpStoreSliceVector    -- ^ Slice after a pack/filter (taking a @Nat@ for new length)

        -- Streams.
        | OpStoreNext           -- ^ Take the next element from a series.
        deriving (Eq, Ord, Show)


