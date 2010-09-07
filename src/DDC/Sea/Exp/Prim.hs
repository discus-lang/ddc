{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Sea.Exp.Prim
	( Prim		(..)
	, PrimOp	(..)
	, PrimProj	(..)
	, PrimFun	(..))
where
import DDC.Sea.Exp.Type


-- | Primitive operators implemented directly in the C language or runtime system.
--	We keep these separate from the Core Op type because the two languages
--	might implement different operators.
data	Prim
	-- | Invoke a primitive arithmetic operator.
	= MOp	PrimOp
	
	-- | Primitive field projections.
	| MProj	PrimProj

	-- | Call some other primitive function in the runtime system.
	| MFun	PrimFun
	deriving (Show, Eq)


-- | Primitive operators.
--   We expect the backend to be able to implement these directly.
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
	= PFunArrayPeek Type
	| PFunArrayPoke Type
	| PFunStrCmp
	deriving (Show, Eq)
	
	
