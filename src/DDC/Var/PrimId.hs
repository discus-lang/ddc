
-- | Primitive variable identifiers.
module DDC.Var.PrimId 
	(PrimId	(..))
where
import DDC.Base.DataFormat

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
	deriving (Eq, Show, Ord)

