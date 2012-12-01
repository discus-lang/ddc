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
	TObj			-> hash (0  :: Int) 
	TData			-> hash (1  :: Int) 
	TThunk			-> hash (2  :: Int) 
	TTuple i		-> hash (3  :: Int)   + hash i
	TList			-> hash (4  :: Int) 
	TRef			-> hash (5  :: Int) 
	TVoidU			-> hash (6  :: Int) 
	TPtrU			-> hash (7  :: Int) 
	TUnit			-> hash (8  :: Int) 
	TBool   fmt		-> hash (10 :: Int) + hash fmt
	TWord   fmt		-> hash (11 :: Int) + hash fmt
	TInt	fmt		-> hash (11 :: Int) + hash fmt
	TFloat	fmt		-> hash (11 :: Int) + hash fmt
	TChar	fmt		-> hash (11 :: Int) + hash fmt
	TString	fmt		-> hash (11 :: Int) + hash fmt
	FConst			-> hash (12 :: Int)
	FConstT			-> hash (13 :: Int)
	FMutable		-> hash (14 :: Int)
	FMutableT		-> hash (15 :: Int)
	FLazy			-> hash (16 :: Int)
	FLazyT			-> hash (17 :: Int)
	FLazyH			-> hash (18 :: Int)
	FDirect			-> hash (19 :: Int)
	FDirectT		-> hash (20 :: Int)
	FPure			-> hash (21 :: Int)
	FEmpty			-> hash (22 :: Int)
	FProj			-> hash (23 :: Int)
	FShape i		-> hash (24 :: Int) + hash i
	VNegate			-> hash (25 :: Int)
	VTuple i		-> hash (26 :: Int) + hash i
	VUnit			-> hash (27 :: Int)
	VTrue	 		-> hash (28 :: Int)
	VFalse			-> hash (29 :: Int)
	VSuspend i		-> hash (30 :: Int) + hash i
	VProjField		-> hash (31 :: Int)
	VProjFieldR		-> hash (32 :: Int)
	VIndex 			-> hash (33 :: Int)
	VIndexR			-> hash (34 :: Int)
	VCons			-> hash (35 :: Int)
	VNil			-> hash (36 :: Int)
	VAppend			-> hash (37 :: Int)
	VExceptionBreak		-> hash (38 :: Int)
	VGateLoop		-> hash (39 :: Int)
	VTry			-> hash (40 :: Int)
	VThrow			-> hash (41 :: Int)
	VWhile			-> hash (42 :: Int)
	VWhen			-> hash (43 :: Int)
	VUnless			-> hash (44 :: Int)
	VRange			-> hash (45 :: Int)
	VRangeL			-> hash (46 :: Int)
	VRangeInfL		-> hash (47 :: Int)
	VRangeIntStep		-> hash (48 :: Int)
	VRangeIntStepL		-> hash (49 :: Int)
	VRangeInfIntStepL	-> hash (50 :: Int)
	VConcatMap		-> hash (51 :: Int)
	VConcatMapL		-> hash (52 :: Int)
	VBind			-> hash (53 :: Int)
	VEq			-> hash (54 :: Int)
	VBoxString		-> hash (55 :: Int)


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
	
