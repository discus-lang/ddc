{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Base.Prim.PrimCall
	(PrimCall(..))
where
import DDC.Var

-- | Primitive ways of invoking a function.
--   TODO: get rid of the Var. This should go in the enclosing type.
data PrimCall
	-- | Tailcall a supercombinator.
	= PrimCallTail		Var

	-- | Call a supercombinator.
	| PrimCallSuper 	Var

	-- | Call a supercombinator then apply the resulting thunk with this arity.
	| PrimCallSuperApply	Var	Int

	-- | Build a thunk with this arity
	| PrimCallCurry		Var	Int

	-- | Apply a thunk.
	| PrimCallApply		Var
	deriving (Show, Eq)
