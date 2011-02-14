{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Sea.Exp.Prim
	( Prim		(..)
	, PrimOp	(..)
	, PrimApp	(..)
	, PrimAlloc	(..)
	, PrimProj	(..)
	, PrimFun	(..))
where
import DDC.Base.Prim
import DDC.Sea.Exp.Type
import DDC.Var

-- | Primitive operators implemented directly in the C language or runtime system.
--   TODO: Merge this with DDC.Core.Exp.Prim
data	Prim
	-- | Box some unboxed value, given the type of the unboxed version.
	= MBox		Type

	-- | Unbox some boxed value, given the type of the unboxed version.
	| MUnbox	Type

	-- | Invoke a primitive arithmetic operator.
	| MOp		PrimOp

	-- | Casting between numeric types.
	| MCast		PrimCast

	-- | Coersion between pointer types.
	--   The arguments give the type of the pointed-to data.
	| MCoerce	(PrimCoerce Type)

	-- | Pointer operations
	| MPtr		PrimPtr

	-- | Primitive operators concerned with function application.
	--   TODO: Change this to use PrimCall
	| MApp		PrimApp
	
	-- | Primitive field projections.
	| MProj		PrimProj

	-- | Call some other primitive function in the runtime system.
	| MFun		PrimFun

	-- | Allocation of objects
	| MAlloc	PrimAlloc
	deriving (Show, Eq)


-- | Primitive operators concerned with function application.
--   TODO: Merge this with PrimCall
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


