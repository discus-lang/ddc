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
 {-# INLINE hashWithSalt #-}
 hashWithSalt s pid
  = case pid of
	TObj			-> hashWithSalt s (0  :: Int) 
	TData			-> hashWithSalt s (1  :: Int) 
	TThunk			-> hashWithSalt s (2  :: Int) 
	TTuple i		-> hashWithSalt s (3  :: Int) + hashWithSalt s i
	TList			-> hashWithSalt s (4  :: Int) 
	TRef			-> hashWithSalt s (5  :: Int) 
	TVoidU			-> hashWithSalt s (6  :: Int) 
	TPtrU			-> hashWithSalt s (7  :: Int) 
	TUnit			-> hashWithSalt s (8  :: Int) 
	TBool   fmt		-> hashWithSalt s (10 :: Int) + hashWithSalt s fmt
	TWord   fmt		-> hashWithSalt s (11 :: Int) + hashWithSalt s fmt
	TInt	fmt		-> hashWithSalt s (11 :: Int) + hashWithSalt s fmt
	TFloat	fmt		-> hashWithSalt s (11 :: Int) + hashWithSalt s fmt
	TChar	fmt		-> hashWithSalt s (11 :: Int) + hashWithSalt s fmt
	TString	fmt		-> hashWithSalt s (11 :: Int) + hashWithSalt s fmt
	FConst			-> hashWithSalt s (12 :: Int)
	FConstT			-> hashWithSalt s (13 :: Int)
	FMutable		-> hashWithSalt s (14 :: Int)
	FMutableT		-> hashWithSalt s (15 :: Int)
	FLazy			-> hashWithSalt s (16 :: Int)
	FLazyT			-> hashWithSalt s (17 :: Int)
	FLazyH			-> hashWithSalt s (18 :: Int)
	FDirect			-> hashWithSalt s (19 :: Int)
	FDirectT		-> hashWithSalt s (20 :: Int)
	FPure			-> hashWithSalt s (21 :: Int)
	FEmpty			-> hashWithSalt s (22 :: Int)
	FProj			-> hashWithSalt s (23 :: Int)
	FShape i		-> hashWithSalt s (24 :: Int) + hashWithSalt s i
	VNegate			-> hashWithSalt s (25 :: Int)
	VTuple i		-> hashWithSalt s (26 :: Int) + hashWithSalt s i
	VUnit			-> hashWithSalt s (27 :: Int)
	VTrue	 		-> hashWithSalt s (28 :: Int)
	VFalse			-> hashWithSalt s (29 :: Int)
	VSuspend i		-> hashWithSalt s (30 :: Int) + hashWithSalt s i
	VProjField		-> hashWithSalt s (31 :: Int)
	VProjFieldR		-> hashWithSalt s (32 :: Int)
	VIndex 			-> hashWithSalt s (33 :: Int)
	VIndexR			-> hashWithSalt s (34 :: Int)
	VCons			-> hashWithSalt s (35 :: Int)
	VNil			-> hashWithSalt s (36 :: Int)
	VAppend			-> hashWithSalt s (37 :: Int)
	VExceptionBreak		-> hashWithSalt s (38 :: Int)
	VGateLoop		-> hashWithSalt s (39 :: Int)
	VTry			-> hashWithSalt s (40 :: Int)
	VThrow			-> hashWithSalt s (41 :: Int)
	VWhile			-> hashWithSalt s (42 :: Int)
	VWhen			-> hashWithSalt s (43 :: Int)
	VUnless			-> hashWithSalt s (44 :: Int)
	VRange			-> hashWithSalt s (45 :: Int)
	VRangeL			-> hashWithSalt s (46 :: Int)
	VRangeInfL		-> hashWithSalt s (47 :: Int)
	VRangeIntStep		-> hashWithSalt s (48 :: Int)
	VRangeIntStepL		-> hashWithSalt s (49 :: Int)
	VRangeInfIntStepL	-> hashWithSalt s (50 :: Int)
	VConcatMap		-> hashWithSalt s (51 :: Int)
	VConcatMapL		-> hashWithSalt s (52 :: Int)
	VBind			-> hashWithSalt s (53 :: Int)
	VEq			-> hashWithSalt s (54 :: Int)
	VBoxString		-> hashWithSalt s (55 :: Int)


-- | For PrimIds that contain a `DataFormat`, split them 
--   into the constructor and said format.
splitPrimIdWithDataFormat 
        :: PrimId 
        -> Maybe (DataFormat -> PrimId, DataFormat)

splitPrimIdWithDataFormat pid
 = case pid of
	TBool	fmt	-> Just (TBool,   fmt)
	TWord	fmt	-> Just (TWord,   fmt)
	TInt	fmt	-> Just (TInt,    fmt)
	TFloat 	fmt	-> Just (TFloat,  fmt)
	TChar	fmt	-> Just (TChar,   fmt)
	TString fmt	-> Just (TString, fmt)
	_		-> Nothing
	
