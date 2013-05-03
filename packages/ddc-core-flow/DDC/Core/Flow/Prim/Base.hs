
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
        -- | Flow kind constructors.
        | NameKiConFlow         KiConFlow

        -- | Flow type constructors.
        | NameTyConFlow         TyConFlow

        -- | Baked in data constructors.
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
        -- | An unboxed boolean literal
        | NameLitBool           Bool

        -- | An unboxed natural literal.
        | NameLitNat            Integer

        -- | An unboxed integer literal.
        | NameLitInt            Integer

        -- | An unboxed word literal
        | NameLitWord           Integer Int
        deriving (Eq, Ord, Show, Typeable)


-- | Flow kind constructors.
data KiConFlow
        = KiConFlowNatP
        | KiConFlowRate
        deriving (Eq, Ord, Show)


-- | Flow type constructors.
data TyConFlow
        -- | @TN@      type constructor.
        = TyConFlowTuple Int            

        -- | @Array#@  type constructor.
        | TyConFlowArray

        -- | @Vector#@ type constructor.
        | TyConFlowVector

        -- | @Stream#@ type constructor.
        | TyConFlowStream

        -- | @Segd#@   type constructor.
        | TyConFlowSegd

        -- | @SelN#@   type constructor.
        | TyConFlowSel Int

        -- | @Ref#@    type constructor.
        | TyConFlowRef                  

        -- | @World#@  state token used when converting to GHC core.
        | TyConFlowWorld

        -- | @RateNat#@ type constructor.          
        | TyConFlowRateNat
        deriving (Eq, Ord, Show)


-- | Primitive data constructors.
data DaConFlow
        = DaConFlowTuple Int            -- ^ @TN@ data constructor.
        deriving (Eq, Ord, Show)


-- | Flow operators.
data OpFlow
        -- Stream conversions.
        = OpFlowStreamOfVector
        | OpFlowVectorOfStream
        | OpFlowRateOfStream

        -- Vector conversions
        | OpFlowArrayOfVector
        | OpFlowVectorOfArray Int

        -- selectors
        | OpFlowMkSel Int

        -- maps
        | OpFlowMap Int

        -- replicates
        | OpFlowRep
        | OpFlowReps

        -- folds
        | OpFlowFold
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
        deriving (Eq, Ord, Show)


-- | Store operators.
data OpStore
        -- Assignables.
        = OpStoreNew            -- ^ Allocate a new assignable.
        | OpStoreRead           -- ^ Read from an assignable.
        | OpStoreWrite          -- ^ Write to an assignable.

        -- Arrays.
        | OpStoreNewArray       -- ^ Allocate a new array.
        | OpStoreReadArray      -- ^ Read from an array.
        | OpStoreWriteArray     -- ^ Write to an array.

        -- Streams.
        | OpStoreNext           -- ^ Take the next element from a stream.
        deriving (Eq, Ord, Show)


