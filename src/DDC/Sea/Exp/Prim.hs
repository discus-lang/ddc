{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Sea.Exp.Prim
	(Prim(..))
where
import DDC.Sea.Exp.Type


-- | Primitive operators implemented directly in the C language or runtime system.
--	We keep these separate from the Core Op type because the two languages
--	might implement different operators.
data	Prim
	= FNeg
	| FAdd
	| FSub
	| FMul
	| FDiv
	| FMod

	| FEq
	| FNEq

	| FGt
	| FLt

	| FGe
	| FLe

	| FAnd
	| FOr

	| FProjField
	| FProjFieldR

	| FArrayPeek Type
	| FArrayPoke Type

	| FStrCmp
	deriving (Show, Eq)
