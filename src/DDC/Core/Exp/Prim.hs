{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Core.Exp.Prim
	( Prim 		(..)
	, PrimCall	(..)
	, PrimOp	(..)
	, takeTypeOfPrimType
	, takePrimTypeOfType)
where
import DDC.Base.Prim
import DDC.Type
import DDC.Base.DataFormat
import DDC.Var
import DDC.Var.PrimId
import Shared.VarPrim


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
		
	-- | Call a function / supercombinator.
	| MCall 	!PrimCall
	deriving (Show, Eq)


-- | Take the `Type` of a `PrimType`
takeTypeOfPrimType :: PrimType -> Maybe Type
takeTypeOfPrimType pt
 = case pt of
	PrimTypeWord  (Width w)	-> Just $ makeTData (primTWord  (UnboxedBits w)) kValue []
	PrimTypeInt   (Width w)	-> Just $ makeTData (primTInt   (UnboxedBits w)) kValue []
	PrimTypeFloat (Width w)	-> Just $ makeTData (primTFloat (UnboxedBits w)) kValue []
	_			-> Nothing

-- | Take the `PrimType` of an unboxed `Type`.
takePrimTypeOfType :: Type -> Maybe PrimType
takePrimTypeOfType tt
	| Just (v, _, [])	<- takeTData tt
	= case varId v of
		VarIdPrim (TWord  (UnboxedBits w))	-> Just $ PrimTypeWord  (Width w)
		VarIdPrim (TInt   (UnboxedBits w))	-> Just $ PrimTypeInt   (Width w)
		VarIdPrim (TFloat (UnboxedBits w))	-> Just $ PrimTypeFloat (Width w)
		_					-> Nothing

	| otherwise		= Nothing
