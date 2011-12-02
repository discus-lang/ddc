{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Primitive variable identifiers.
module DDC.Var.PrimId 
	( PrimId	(..)
	, splitPrimIdWithDataFormat)
where
import DDC.Base.DataFormat
import Data.Hashable

-- | Variables with these idenfiers have special meaning to the compiler.
data PrimId
	-- Primitive low level types
	= TObj
	| TData
	| TThunk
	
	-- Primitive data constructors
	| TTuple Int
	| TList
	| TRef
	
	-- Primitive data types
	| TVoidU
	| TPtrU
	| TUnit	
	| TBool   DataFormat
	| TWord   DataFormat
	| TInt	  DataFormat
	| TFloat  DataFormat
	| TChar	  DataFormat
	| TString DataFormat
		
	-- Class constructors
	| FConst
	| FConstT
	| FMutable
	| FMutableT
	| FLazy
	| FLazyT
	| FLazyH
	| FDirect
	| FDirectT
	| FPure
	| FEmpty
	| FProj
	| FShape  Int

	-- Value variables
	| VNegate
	| VTuple Int		
	| VUnit
	| VTrue	 
	| VFalse
	| VSuspend Int
	| VProjField
	| VProjFieldR
		
	-- For desugaring bulk collection indexing
	| VIndex 	
	| VIndexR

	-- For desugaring list comprehensions
	| VCons
	| VNil
	| VAppend
	
	-- For desugaring while/break
	| VExceptionBreak	
	| VGateLoop
	
	-- For desugaring exceptions
	| VTry
	| VThrow
	
	-- For desugaring imperative sugar.
	| VWhile
	| VWhen
	| VUnless

	-- For desugaring list ranges
	| VRange	
	| VRangeL
	| VRangeInfL

	| VRangeIntStep
	| VRangeIntStepL
	| VRangeInfIntStepL
	
	-- For desugaring list comprehensions
	| VConcatMap	
	| VConcatMapL

	-- The monadic bind
	| VBind		

	-- For desugaring literal pattern matches
	| VEq

	| VBoxString
	deriving (Eq, Show, Ord)


-- This is a bit tragic.
-- We can't derive Enum because some of the constructors have arguments.

instance Hashable PrimId where
 {-# INLINE hash #-}
 hash pid
  = case pid of
	TObj			-> hashInt 0
	TData			-> hashInt 1
	TThunk			-> hashInt 2
	TTuple i		-> hashInt 3  + hash i
	TList			-> hashInt 4
	TRef			-> hashInt 5
	TVoidU			-> hashInt 6
	TPtrU			-> hashInt 7
	TUnit			-> hashInt 8
	TBool   fmt		-> hashInt 10 + hash fmt
	TWord   fmt		-> hashInt 11 + hash fmt
	TInt	fmt		-> hashInt 11 + hash fmt
	TFloat	fmt		-> hashInt 11 + hash fmt
	TChar	fmt		-> hashInt 11 + hash fmt
	TString	fmt		-> hashInt 11 + hash fmt
	FConst			-> hashInt 12
	FConstT			-> hashInt 13
	FMutable		-> hashInt 14
	FMutableT		-> hashInt 15
	FLazy			-> hashInt 16
	FLazyT			-> hashInt 17
	FLazyH			-> hashInt 18
	FDirect			-> hashInt 19
	FDirectT		-> hashInt 20
	FPure			-> hashInt 21
	FEmpty			-> hashInt 22
	FProj			-> hashInt 23
	FShape i		-> hashInt 24 + hash i
	VNegate			-> hashInt 25
	VTuple i		-> hashInt 26 + hash i
	VUnit			-> hashInt 27
	VTrue	 		-> hashInt 28
	VFalse			-> hashInt 29
	VSuspend i		-> hashInt 30 + hash i
	VProjField		-> hashInt 31
	VProjFieldR		-> hashInt 32
	VIndex 			-> hashInt 33
	VIndexR			-> hashInt 34
	VCons			-> hashInt 35
	VNil			-> hashInt 36
	VAppend			-> hashInt 37
	VExceptionBreak		-> hashInt 38
	VGateLoop		-> hashInt 39
	VTry			-> hashInt 40
	VThrow			-> hashInt 41
	VWhile			-> hashInt 42
	VWhen			-> hashInt 43
	VUnless			-> hashInt 44
	VRange			-> hashInt 45
	VRangeL			-> hashInt 46
	VRangeInfL		-> hashInt 47
	VRangeIntStep		-> hashInt 48
	VRangeIntStepL		-> hashInt 49
	VRangeInfIntStepL	-> hashInt 50
	VConcatMap		-> hashInt 51
	VConcatMapL		-> hashInt 52
	VBind			-> hashInt 53
	VEq			-> hashInt 54
	VBoxString		-> hashInt 55


-- | For PrimIds that contain a `DataFormat`, split them 
--   into the constructor and said format.
splitPrimIdWithDataFormat :: PrimId -> Maybe (DataFormat -> PrimId, DataFormat)
splitPrimIdWithDataFormat pid
 = case pid of
	TBool	fmt	-> Just (TBool,   fmt)
	TWord	fmt	-> Just (TWord,   fmt)
	TInt	fmt	-> Just (TInt,    fmt)
	TFloat 	fmt	-> Just (TFloat,  fmt)
	TChar	fmt	-> Just (TChar,   fmt)
	TString fmt	-> Just (TString, fmt)
	_		-> Nothing
	
