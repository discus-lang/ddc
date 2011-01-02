{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Base.Prim.PrimOp
	( PrimOp(..)
	, readPrimOp )
where
import DDC.Base.Prim.PrimType
import Data.List

-- | Primitive polymorphic operators. 
--   We expect the backend to be able to implement these directly.
data PrimOp
	-- arithmetic
	= OpNeg
	| OpAdd
	| OpSub
	| OpMul
	| OpDiv
	| OpMod

	-- tests
	| OpIsZero

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


-- | Primitive operators in the source language have names like "primWord32U_neg"
--   This splits off the "_neg" part and returns the associated `PrimOp` (ie `OpNeg`).
readPrimOp :: String -> Maybe (PrimOp, PrimType)
readPrimOp str
 	| Just typeOpName		<- stripPrefix "prim" str
	, (typeName, _ : opName)	<- break (== '_') typeOpName
	, Just pt			<- readPrimType typeName
	, Just op			<- lookup opName   primOpNames
	= Just (op, pt)

	| otherwise
	= Nothing


-- | Primitive operators in the source language have names like "primWord32U_neg".
--   This converts the "neg" part to the `PrimOp` it refers to.
primOpNames
	-- arithmetic
 =	[ ("neg",	OpNeg)
	, ("add",	OpAdd)
	, ("sub",	OpSub)
	, ("mul",	OpMul)
	, ("div",	OpDiv)
	, ("mod",	OpMod)

	-- tests
	, ("isZero",	OpIsZero)
	
	-- comparison
	, ("eq",	OpEq)
	, ("neq",	OpNeq)
	, ("gt",	OpGt)
	, ("ge",	OpGe)
	, ("lt",	OpLt)
	, ("le",	OpLe)

	-- boolean
	, ("and",	OpAnd)
	, ("or",	OpOr) ]

