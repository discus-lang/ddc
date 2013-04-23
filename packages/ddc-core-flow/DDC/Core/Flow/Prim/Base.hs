
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
        = TyConFlowNatP  Int
        | TyConFlowLen

        | TyConFlowTuple Int            -- ^ @TN@      type constructor.
        | TyConFlowArray                -- ^ @Array#@  type constructor.
        | TyConFlowVector               -- ^ @Vector#@ type constructor.
        | TyConFlowStream               -- ^ @Stream#@ type constructor.
        | TyConFlowSegd                 -- ^ @Segd#@   type constructor.
        | TyConFlowSel Int              -- ^ @SelN#@   type constructor.

        | TyConFlowRef                  -- ^ @Ref#@    type constructor.
        deriving (Eq, Ord, Show)


-- | Primitive data constructors.
data DaConFlow
        = DaConFlowTuple Int            -- ^ @TN@ data constructor.
        deriving (Eq, Ord, Show)


-- | Flow operators.
data OpFlow
        -- conversion
        = OpFlowToStream
        | OpFlowFromStream
        | OpFlowLengthOfStream

        | OpFlowToVector        Int
        | OpFlowFromVector

        -- rate conversion
        | OpFlowLengthOfRate    

        -- selectors
        | OpFlowMkSel           Int

        -- maps
        | OpFlowMap              Int

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
        | OpFlowSplit           Int
        | OpFlowCombine         Int

        -- packing
        | OpFlowPack
        deriving (Eq, Ord, Show)


-- | Loop operators.
data OpLoop
        = OpLoopLoop
        deriving (Eq, Ord, Show)


-- | Store operators.
data OpStore
        = OpStoreNew                    -- ^ @new#@,   allocate a new array.
        | OpStoreRead                   -- ^ @read#@,  read from an array.
        | OpStoreWrite                  -- ^ @write#@, write to an array.

        | OpStoreNext                   -- ^ @next#@,  take the next element of a stream.
        deriving (Eq, Ord, Show)


