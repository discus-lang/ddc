{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Core.Exp.Prim
	( Prim 		(..)
	, PrimCall	(..)
	, PrimOp	(..))
where
import DDC.Base.Prim
import DDC.Type


-- | Primitive functions.
--   TODO: Abstract this over the `Type` so we can use it for the Sea language as well.
data Prim
	-- | Force the outer constructor of an expression.
	= MForce

	-- | Box some value.
	| MBox		!PrimType
	
	-- | Unbox some value.
	| MUnbox	!PrimType

	-- | A primitive comparison, numeric, or logic operator.
	| MOp		!PrimType	!PrimOp
	
	-- | Casting between primitive types,
	--   eg between Int32# and Float32#.
	| MCast		!PrimCast
	
	-- | Coercion between unboxed pointer types.
	| MCoerce	!(PrimCoerce Type)
	
	-- | Pointer operations
	| MPtr		!PrimPtr
		
	-- | Call a function / supercombinator.
	| MCall 	!PrimCall
	deriving (Show, Eq)

