{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Base.Prim.PrimOp
	( PrimOp(..)
	, primOpNames
	, readPrimOpName
	, readPrimOp )
where
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


-- | Primitive operators in the source language have names like "primWord32U_neg".
--   This converts the "neg" part to the `PrimOp` it refers to.
readPrimOpName :: String -> Maybe PrimOp
readPrimOpName str
	= lookup str primOpNames

primOpNames
	-- arithmetic
 =	[ ("neg",	OpNeg)
	, ("add",	OpAdd)
	, ("sub",	OpSub)
	, ("mul",	OpMul)
	, ("div",	OpDiv)
	, ("mod",	OpMod)

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


-- | Primitive operators in the source language have names like "primWord32U_neg"
--   This splits off the "_neg" part and returns the associated `PrimOp` (ie `OpNeg`).
readPrimOp :: String -> Maybe PrimOp
readPrimOp str
 	| Just typeOpName		<- stripPrefix "prim" str
	, (typeName, _ : opName)	<- break (== '_') typeOpName
	, elem typeName [ "PtrVoid"
			, "Word32U",  "Word64U"
			, "Int32U",   "Int64U"
			, "Float32U", "Float64U"]
	= readPrimOpName opName

	| otherwise
	= Nothing



