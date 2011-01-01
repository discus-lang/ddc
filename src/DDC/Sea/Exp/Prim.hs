{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Sea.Exp.Prim
	( Prim		(..)
	, PrimOp	(..)
	, PrimApp	(..)
	, PrimAlloc	(..)
	, PrimProj	(..)
	, PrimFun	(..))
where
import DDC.Base.Prim.PrimOp
import DDC.Base.Prim.PrimType
import DDC.Sea.Exp.Type
import DDC.Var

-- | Primitive operators implemented directly in the C language or runtime system.
data	Prim
	-- | Invoke a primitive arithmetic operator.
	= MOp	PrimOp

	-- | Primitive field projections.
	| MProj	PrimProj

	-- | Primitive operators concerned with function application.
	| MApp  PrimApp

	-- | Call some other primitive function in the runtime system.
	| MFun	PrimFun

	-- | Allocation of objects
	| MAlloc PrimAlloc

	-- | Box some unboxed value, given the type of the unboxed version.
	| MBox	Type

	-- | Unbox some boxed value, given the type of the unboxed version.
	| MUnbox Type

	-- | Casting between numeric types.
	| MCast  PrimType PrimType

	-- | Coersion between pointer types.
	--   The arguments give the type of the pointed-to data.
	| MCoercePtr Type Type

	deriving (Show, Eq)


-- | Primitive operators concerned with function application.
data PrimApp
	-- | Tail-call a super.
	= PAppTailCall

	-- | Call a super directly.
	| PAppCall

	-- | Call a super to yield a thunk then apply it to some more args.
	| PAppCallApp Int	-- super arity. TODO: determine this from the type of the super.

	-- | Perform a general function application.
	| PAppApply

	-- | Build a thunk containing a pointer to some super.
	| PAppCurry Int		-- super arity. TODO: determine this from the type of the super.
	deriving (Show, Eq)


-- | Allocation of objects.
data PrimAlloc
	-- | Allocate a fresh thunk object,
	--   and fill in the super pointer, super arity, and number of args in the thunk.
	= PAllocThunk Var Type Int Int

	-- | Allocate a fresh data object,
	--   and fill in the constructor tag and arity.
	| PAllocData Var Int
	deriving (Show, Eq)


-- | Primitive projections.
--   These may be implemented by a function call depending on the backend.
data PrimProj
	-- | Project out the value of a field.
	= PProjField

	-- | Take a reference to a field.
	| PProjFieldRef
	deriving (Show, Eq)


-- | Calls to primitive functions.
--   TODO: We probably want to ditch this and rewrite to call the functions directly.
data PrimFun
	= PFunForce
	| PFunArrayPeek Type
	| PFunArrayPoke Type
	| PFunStrCmp
	deriving (Show, Eq)


