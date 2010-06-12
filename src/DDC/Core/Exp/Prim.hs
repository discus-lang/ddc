{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Core.Exp.Prim
	( Prim 		(..)
	, PrimCall	(..)
	, PrimOp	(..))
where

-- | Primitive functions.
--   These are polymorphic primitives that we deal with directly in the core language.
--   Exposing these makes it easier to perform rewrites. If we had a proper rule rewriting
--   system we could handle them more generally.
data Prim
	-- | Force the outer constructor of an expression.
	= MForce

	-- | Box some value.
	| MBox
	
	-- | Unbox some value.
	| MUnbox

	-- | Invoke a primitive operator.
	| MOp		PrimOp
	
	-- | Some function-call related thing.
	| MCall 	PrimCall
	deriving (Show, Eq)


-- | Primitive ways of invoking a function.
data PrimCall
	-- | Tailcall a supercombinator.
	= PrimCallTail

	-- | Call a supercombinator.
	| PrimCallSuper

	-- | Call a supercombinator then apply the resulting thunk with this arity.
	| PrimCallSuperApply	Int

	-- | Apply a thunk.
	| PrimCallApply

	-- | Build a thunk with this arity
	| PrimCallCurry		Int
	deriving (Show, Eq)
	

-- | Primitive polymorphic operators.
--	These are things we might want to handle specially during core rewrites.
--	If we had a proper rule rewriting system we could handle these more generally.
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

