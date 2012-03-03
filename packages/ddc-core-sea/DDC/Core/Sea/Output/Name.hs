
-- | Names used in the SeaOutput language profile.
--   These map directly onto names used in the C output language.
module DDC.Core.Sea.Output.Name
        ( Name          (..)
        , Prim          (..)
        , PrimTyCon     (..)
        , PrimOp        (..)
        , PrimProj      (..)
        , PrimCast      (..)
        , PrimCall      (..)
        , PrimAlloc     (..)
        , readName)
where
import DDC.Core.Sea.Base.Name   (PrimOp(..), primOpNames)
import Data.Char
import Data.List


-- Names of things recognised by the Sea backend.
data Name
        -- | A type constructor.
        = NamePrimTyCon PrimTyCon

        -- | A variable on the stack.
        | NameAuto      String

        -- | A function defined at top-level.
        | NameFun       String

        -- | A primitive operator.
        | NamePrim      Prim

        -- | A integer literal.
        | NameInt       Integer
        deriving (Eq, Show)


-- PrimTyCon -----------------------------------------------------------------
-- | Primitive type constructors.
data PrimTyCon
        -- | Type of machine addresses.
        = PrimTyConAddr

        -- | Unsigned words of the given length.
        | PrimTyConWord   Int

        -- | Signed integers of the given length.
        | PrimTyConInt    Int

        -- | Floating point numbers of the given length.
        | PrimTyConFloat  Int
        deriving (Show, Eq)


-- Prim -----------------------------------------------------------------------
-- | Primitive operators implemented directly by the machine or runtime system.
data    Prim
        -- | Invoke a primitive arithmetic operator.
        = PrimOp        PrimOp

        -- | Project something from an object.
        | PrimProj      PrimProj

        -- | Casting between numeric types.
        | PrimCast      PrimCast

        -- | Calling functions, internal or external.
        | PrimCall      PrimCall

        -- | Allocate an object.
        | PrimAlloc     PrimAlloc
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
        = PrimCastOf PrimTyCon PrimTyCon
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


-- Parsing --------------------------------------------------------------------
readName :: String -> Maybe Name
readName []     = Nothing
readName str@(c:_)
        -- primops
        | isLower c
        , Just (op, _) <- find (\(_, name) -> str == name) primOpNames
        = Just $ NamePrim $ PrimOp op

        | otherwise
        = Nothing

