
module DDC.Core.Sea.Name
        ( Name          (..)
        , TyConPrim     (..)
        , Prim          (..)
        , PrimOp        (..)
        , PrimProj      (..)
        , PrimCast      (..)
        , PrimCall      (..)
        , PrimAlloc     (..))
where


data Name
        = NameBound     String
        | NameAuto      String
        | NamePrim      Prim
        deriving (Eq, Show)


-- TyConPrim -----------------------------------------------------------------
-- | Primitive type constructors.
data TyConPrim
        -- | Type of machine addresses.
        = TyConPrimAddr

        -- | Unsigned words of the given length.
        | TyConPrimWord   Int

        -- | Signed integers of the given length.
        | TyConPrimInt    Int

        -- | Floating point numbers of the given length.
        | TyConPrimFloat  Int
        deriving (Show, Eq)


-- Prim -----------------------------------------------------------------------
-- | Primitive operators implemented directly by the machine or runtime system.
data    Prim
        -- | Invoke a primitive arithmetic operator.
        = MOp           PrimOp

        -- | Project something from an object.
        | MProj         PrimProj

        -- | Casting between numeric types.
        | MCast         PrimCast

        -- | Calling functions, internal or external.
        | MCall         PrimCall

        -- | Allocate an object.
        | MAlloc        PrimAlloc
        deriving (Show, Eq)


-- PrimOp ---------------------------------------------------------------------
-- | Primitive numeric, comparison or logic operators.
--   We expect the backend/machine to be able to implement these directly.
data PrimOp
        -- arithmetic
        = OpNeg
        | OpAdd
        | OpSub
        | OpMul
        | OpDiv
        | OpMod

        -- comparison
        | OpEq
        | OpNeq
        | OpGt
        | OpGe
        | OpLt
        | OpLe

        -- boolean
        | OpAnd
        | OpOr
        deriving (Show, Eq)


-- Proj -----------------------------------------------------------------------
-- | A projection of some other object.
data PrimProj
        -- | Take the tag of a boxed object.
        = PrimProjTag

        -- | Take a numbered field from some boxed data object.
        | PrimProjField
        deriving (Show, Eq)


-- PrimCast -------------------------------------------------------------------
-- | Primitive cast between two types.
data PrimCast
        = PrimCast TyConPrim TyConPrim
        deriving (Show, Eq)


-- PrimCall -------------------------------------------------------------------
-- | Primitive ways of invoking a function.
data PrimCall
        -- | Tailcall a top level function..
        = PrimCallTail

        -- | Call a top level function
        | PrimCallSuper

        -- | Call a supercombinator then apply the resulting thunk.
        | PrimCallSuperApply

        -- | Build a thunk.
        | PrimCallCurry

        -- | Apply a thunk.
        | PrimCallApply
        deriving (Show, Eq)


-- PrimAlloc ------------------------------------------------------------------
-- | Allocation of objects.
data PrimAlloc
        -- | Allocate a fresh thunk object,
        --   and fill in the function pointer, function arity, 
        --   and number of args in the thunk.
        = PAllocThunk

        -- | Allocate a fresh Data object,
        --   and fill in the constructor tag and arity.
        | PAllocData

        -- | Allocate a fresh DataM object
        --   and fill in the number of boxed objects and the DataM payload
        --   size.
        | PAllocDataM
        deriving (Show, Eq)
