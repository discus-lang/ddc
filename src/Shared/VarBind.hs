
module Shared.VarBind
	( VarBind (..)
	, takeVarBind_dataFormat
	, incVarBind)
where
import Shared.Base
import Shared.Pretty


data VarBind
	-- binding not set
	= XNil
	
	-- A regular user-defined var.
	| XBind String Int

	-- Primitive low level types
	| TObj
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
	
	-- Effect constructors
	| ESync
	| ERead
	| EWrite
	| EReadT
	| EReadH
	| EWriteT
	
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
	| VAppend

	| VNil
	
	-- For desugaring while/break
	| VExceptionBreak	
	| VGateLoop
	
	| VTry
	| VThrow
	
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

	-- | The monadic bind
	| VBind		

  -- | For desugaring literal pattern matches
  | VEq
	deriving (Eq, Show, Ord)


instance Pretty VarBind PMode where
 ppr b
  = case b of
  	XBind	s i	-> ppr $ s ++ show i
	_		-> ppr $ show b
	


-- | If this varBind contains an embedded DataFormat, then Just it
takeVarBind_dataFormat :: VarBind -> Maybe DataFormat
takeVarBind_dataFormat bind
 = case bind of
 	TBool fmt	-> Just fmt
	TWord fmt	-> Just fmt
	TInt  fmt	-> Just fmt
	TFloat fmt	-> Just fmt
	TChar fmt	-> Just fmt
	TString fmt	-> Just fmt
	_		-> Nothing


-- | Increment an XBind to the next one
incVarBind :: VarBind -> VarBind
incVarBind b
 = case b of
 	XBind s i	-> XBind s (i + 1)
	_		-> error $ "incVarBind: cannot increment " ++ show b




