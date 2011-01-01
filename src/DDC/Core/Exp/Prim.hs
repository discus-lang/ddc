{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Core.Exp.Prim
	( Prim 		(..)
	, PrimCall	(..)
	, PrimOp	(..)
	, takeTypeOfPrimType)
where
import DDC.Base.Prim.PrimType
import DDC.Base.Prim.PrimOp
import DDC.Type
import DDC.Base.DataFormat
import Shared.VarPrim


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
	
	-- | Casting between primitive types,
	--   eg between Int32# and Float32#.
	| MCast		PrimType  PrimType
	
	-- | Coercion between unboxed pointer types.
	--   eg between (Ptr# (String %r1)) and (Ptr# Word8#)
	--   The arguments give the type of the pointed-to data.
	| MCoercePtr	Type	  Type
	
	-- | Coercion betweeen (Ptr# a) and Addr#
	| MCoercePtrToAddr Type
	
	-- | Coercion between Addr# and (Ptr# a)
	| MCoerceAddrToPtr Type
	
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


-- | Take the `Type` of a `PrimType`
takeTypeOfPrimType :: PrimType -> Maybe Type
takeTypeOfPrimType pt
 = case pt of
	PrimTypeWord  (Width w)	-> Just $ makeTData (primTWord  (UnboxedBits w)) kValue []
	PrimTypeInt   (Width w)	-> Just $ makeTData (primTInt   (UnboxedBits w)) kValue []
	PrimTypeFloat (Width w)	-> Just $ makeTData (primTFloat (UnboxedBits w)) kValue []
	_			-> Nothing


