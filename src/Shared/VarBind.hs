
module Shared.VarBind
(
	VarBind (..),
	incVarBind
)

where

import Util

-----------------------
-- VarCore
-- 	| These variables have special meaning to the compiler.
--
data VarBind
	-- binding not set
	= XNil
	
	-- A regular user-defined var.
	| XBind String Int

	-- Type constructors.
	| TUnit	
	| TBool
	| TChar
	| TInt
	| TFloat
	| TString

	| TTuple Int	-- ^ Tuple type constructor of given airity.
	| TList

	| TObj
	| TData
	| TThunk
	| TRef
	
	-- Primitive types.
	| TVoidU
	| TPtrU

	| TBoolU

	| TWord8U
	| TWord16U
	| TWord32U
	| TWord64U
	
	| TInt8U
	| TInt16U
	| TInt32U
	| TInt64U
	
	| TFloat32U
	| TFloat64U

	| TStringU
	
	-- Effect constructors.
	| ESync		-- :: !

	| ERead		-- :: % -> !
	| EWrite	-- :: % -> !
	
	| EReadT	-- :: * -> !,	read all of object.
	| EReadH	-- :: * -> !,	read head of object.

	| EWriteT	-- :: * -> !
	
	-- Fetter constructors.
	| FConst	-- :: % -> &
	| FMutable	-- :: % -> &
	| FLazy		-- :: % -> &
	| FDirect	-- :: % -> &

	| FConstT	-- :: * -> &
	| FMutableT	-- :: * -> &
	| FDirectT	-- :: * -> &

	| FLazyT	-- :: * -> &
	| FLazyH	-- :: * -> &,	head of object is in a lazy region.
	
	| FPure		-- :: ! -> &
	
--	| FFieldHas	-- :: * -> * -> . -> &
--	| FFieldIs	-- :: * -> * -> . -> &

	| FProj		-- :: * -> * -> . -> +
	
	| FShape Int	--		Shape class. Enforces args to have same structure, while leaving regions free.

	-- Value variables.
	| VTuple Int	-- ^ Tuple data constructor of given airity.

	| VUnit		-- ^ the unit value.

	| VTrue
	| VFalse

	| VSuspend Int	-- ^ Suspension function.
	
	| VProjField
	| VProjFieldR
	
	-- For desugaring bulk collection indexing.
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

	| VRange	-- ^ Used by Source.Hacks to de-sugar list ranges.
	| VRangeL
	| VRangeInfL
	
	| VConcatMap	-- ^ Used by Source.Hacks to de-sugar list comprehensions.
	| VConcatMapL
	deriving (Eq, Show, Ord)


-----
incVarBind ::	VarBind 	-> VarBind
incVarBind	b
 = case b of
 	XBind s i	-> XBind s (i + 1)
	_		-> error $ "incVarBind: cannot increment " ++ show b

----
instance Pretty VarBind where
 pretty b
  = case b of
  	XBind	s i	-> s ++ show i
	_		-> show b
	



