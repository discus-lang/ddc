{-# OPTIONS -O2 #-}

-- | These are the variables that have a special meaning to the compiler.
--   They all need to be defined in the base libraries or runtime system.
module Shared.VarPrim
where
import Util
import DDC.Var
import DDC.Var.PrimId
import DDC.Base.DataFormat
import qualified Data.Map	as Map

	
-- Operational Types -------------------------------------------------------------------------------
primTObj	= primVar 	NameType	"Base.Obj"			TObj
primTData	= primVar 	NameType	"Base.Data"			TData
primTThunk	= primVar	NameType	"Base.Thunk"			TThunk
	
			
-- Type Constructors ------------------------------------------------------------------------
primTVoidU	= primVarI	NameType	"Base.Void#"			TVoidU		[ISeaName "void"]
primTPtrU	= primVarI	NameType	"Base.Ptr#"			TPtrU		[ISeaName "Ptr"]

primTUnit	= primVar	NameType	"Base.Unit"			TUnit

primTBool	= primVarFmt	NameType	"Base.Bool"			TBool
primTWord	= primVarFmt	NameType	"Base.Word"			TWord
primTInt	= primVarFmt	NameType	"Base.Int"			TInt
primTFloat	= primVarFmt	NameType	"Base.Float"			TFloat
primTChar	= primVarFmt	NameType	"Base.Char"			TChar
primTString	= primVarFmt	NameType	"Base.String"			TString

primTRef	= primVar 	NameType	"Base.Ref"			TRef
primTList	= primVar 	NameType	"Data.List.List"		TList
primTTuple i	= primVar	NameType	("Data.Tuple.Tuple" ++ show i) 	(TTuple i)


-- Classes -----------------------------------------------------------------------------------------
primFShape i	= primVar NameClass	("Base.Shape" ++ show i)		(FShape  i)

primLazy	= primVar NameClass	"Base.Lazy"				FLazy
primConst	= primVar NameClass	"Base.Const"				FConst
primMutable	= primVar NameClass	"Base.Mutable"				FMutable
primDirect	= primVar NameClass	"Base.Direct"				FDirect

primConstT	= primVar NameClass	"Base.ConstT"				FConstT
primMutableT	= primVar NameClass	"Base.MutableT"				FMutableT
primDirectT	= primVar NameClass	"Base.DirectT"				FDirectT

primLazyT	= primVar NameClass	"Base.LazyT"				FLazyT
primLazyH	= primVar NameClass	"Base.LazyH"				FLazyH

primPure	= primVar NameClass	"Base.Pure"				FPure
primEmpty	= primVar NameClass	"Base.Empty"				FEmpty


-- Values ------------------------------------------------------------------------------------------
-- from Base
primUnit	= primVar NameValue	"Base.Unit"				VUnit
primTrue	= primVar NameValue 	"Base.True"				VTrue
primFalse	= primVar NameValue	"Base.False"				VFalse

primProjField	= primVarI NameValue	"Base.primProjField"			VProjField
					[ ISeaName "primProjField"]

primProjFieldR	= primVarI NameValue	"Base.primProjFieldR"			VProjFieldR
					[ ISeaName "primProjFieldR"]

primSuspend i	= primVarI NameValue	("Base.Thunk.suspend" ++ show i) 	(VSuspend i)	
					[ISeaName ("primSuspend" ++ show i)]

primExceptionBreak
		= primVar NameValue	"Base.ExceptionBreak"			VExceptionBreak	

-- From Class.Monad
primBind	= primVar NameValue	"Class.Monad.>>="			VBind

-- From Class.Eq
primEq		= primVar NameValue	"Class.Eq.=="				VEq

-- From Control.Exception
primThrow	= primVarI NameValue	"Control.Exception.primThrow"		VThrow
					[ISeaName ("primException_throw")]

primTry		= primVarI NameValue	"Control.Exception.primTry"		VTry
					[ISeaName ("primException_try")]


primGateLoop	= primVar NameValue	"Control.Exception.gateLoop"		VGateLoop

-- From Control.Imperative
primWhile	= primVar NameValue	"Control.Imperative.while"		VWhile	

-- From Data.String
primBoxString	= primVar  NameValue	"Data.String"				VBoxString

-- From Data.Array
primIndex	= primVarI NameValue	"Data.Array.index"			VIndex
					[ISeaName "primArray_index"]
					
primIndexR	= primVarI NameValue	"Data.Array.indexR"			VIndexR
					[ISeaName "primArray_indexR"]

-- From Class.Num
primNegate	= primVar NameValue	"Class.Num.negate"			VNegate

-- From Data.Tuple
primTuple i	= primVar NameValue	("Data.Tuple.Tuple" ++ show i) 		(VTuple i)

-- From Data.List
primNil		= primVar NameValue	"Data.List.Nil"				VNil	
primCons	= primVar NameValue	"Data.List.Cons"			VCons	
primAppend	= primVar NameValue	"Data.List.Append"			VAppend	
primRange	= primVar NameValue	"Data.List.rangeInt" 			VRange
primRangeL	= primVar NameValue	"Data.List.rangeIntL"			VRangeL
primRangeStep	= primVar NameValue	"Data.List.rangeIntStep" 		VRangeIntStep
primRangeStepL	= primVar NameValue	"Data.List.rangeIntStepL"		VRangeIntStepL
primRangeInfStepL = primVar NameValue	"Data.List.rangeInfIntStepL"		VRangeInfIntStepL
primRangeInfL	= primVar NameValue	"Data.List.rangeInfIntL"		VRangeInfL
primConcatMap	= primVar NameValue	"Data.List.concatMap"			VConcatMap
primConcatMapL	= primVar NameValue	"Data.List.concatMapL"			VConcatMapL



-- Utils -------------------------------------------------------------------------------------------
-- | Create a primitive variable
primVar :: NameSpace -> String -> PrimId -> Var
primVar space name pid
 = let	parts		= breakOns '.' name
 	Just modParts	= takeInit parts
	Just vName	= takeLast parts

   in	(varWithName vName)
 	{ varId		= VarIdPrim pid
	, varNameSpace	= space
	, varModuleId	= ModuleId modParts }


-- | Create a primitive variable with some extended info
primVarI :: NameSpace -> String -> PrimId -> [VarInfo] -> Var
primVarI space name pid info
  	= (primVar space name pid) 
		{ varInfo = info }


-- | Create the var for this primitive type
primVarFmt :: NameSpace -> String -> (DataFormat -> PrimId) -> DataFormat -> Var
primVarFmt space name mkBind fmt
 = let	suffix = case fmt of
			Boxed		-> ""
			Unboxed		-> "#"
			BoxedBits d	-> show d
			UnboxedBits d	-> show d ++ "#"
	
	name_parts	= breakOns '.' name
	Just vName	= takeLast name_parts

   in	primVarI 
		space 
		(name ++ suffix) 
		(mkBind fmt)			
		[ISeaName $ vName ++ filter (/= '#') suffix]

-- | If this string has the given prefix then split it off and return the rest
--	of the string
splitPrefix :: Eq a => [a] -> [a] -> Maybe [a]
splitPrefix [] xs	= Just xs

splitPrefix (p:ps) (x:xs)
	| p == x	= splitPrefix ps xs
	| otherwise	= Nothing

splitPrefix (p:ps) []	= Nothing


-- | Check whether a var is for an unboxed type constructor
varIsUnboxedTyConData :: Var -> Bool
varIsUnboxedTyConData var
 	| takeLast (varName var) == Just '#'
	= True
	
	| otherwise
	= varBindIsUnboxedTyConData (varId var)

varBindIsUnboxedTyConData vid
	| Just fmt		<- takeDataFormatOfVarId vid
	= dataFormatIsUnboxed fmt

	| VarIdPrim TVoidU	<- vid	= True
	| VarIdPrim TPtrU	<- vid	= True
	
	| otherwise		= False


-- getPrimVarBind -------------------------------------------------------------------------------------	  
-- | If this var has a special meaning to the compiler then get its unique binder
getPrimVarBind :: NameSpace -> Var -> Maybe VarId
getPrimVarBind space var
 = case space of
  	NameValue	-> renamePrimVar_value  $ varName var
	NameType	-> renamePrimVar_type   $ varName var
	NameClass	-> liftM VarIdPrim $ renamePrimVar_class  $ varName var
	_		-> Nothing
	
renamePrimVar_value :: String -> Maybe VarId
renamePrimVar_value ss
	| Just xx	<- splitPrefix "Tuple" ss
	= Just $ VarIdPrim $ VTuple (read xx)
	
	| Just xx	<- splitPrefix "suspend" ss
	= Just $ VarIdPrim $ VSuspend (read xx)
	
	| otherwise
	= liftM VarIdPrim
	$ Map.lookup ss renamePrimVar_values
	
renamePrimVar_values
	= Map.fromList 
	[ ("Unit",		VUnit)
	, ("True",		VTrue)
	, ("False",		VFalse)
	, ("negate",		VNegate)
	, ("index",		VIndex)
	, ("indexR",		VIndexR)
	, ("primProjField",	VProjField)
	, ("primProjFieldR",	VProjFieldR)
	, ("Nil",		VNil)
	, ("Cons",		VCons)
	, ("Append",		VAppend)
	, ("ExceptionBreak",	VExceptionBreak)
	, ("gateLoop",		VGateLoop)
	, ("primThrow",		VThrow)
	, ("primTry",		VTry)
	, ("rangeInt",		VRange)
	, ("rangeIntL",		VRangeL)
	, ("rangeInfIntL",	VRangeInfL)
	, ("rangeIntStep",	VRangeIntStep)
	, ("rangeIntStepL",	VRangeIntStepL)
	, ("rangeInfIntStepL",	VRangeInfIntStepL)
	, ("concatMap",		VConcatMap)
	, ("concatMapL",	VConcatMapL)
	, (">>=",		VBind)
	, ("==",		VEq)
	, ("boxString",		VBoxString) ]

	
renamePrimVar_type :: String -> Maybe VarId
renamePrimVar_type ss
	| Just xx	<- splitPrefix "Tuple" ss
	= Just (VarIdPrim $ TTuple $ read xx)
	
	| otherwise
	= liftM VarIdPrim 
	$ Map.lookup ss renamePrimVar_types
		
	
-- | We manually list out all the primitive types to ensure
--	that invalid ones are not bound.	
renamePrimVar_types 
	= Map.fromList
	[ ("Obj",		TObj)
	, ("Data",		TData)
	, ("Thunk",		TThunk)

	, ("Void#",		TVoidU)
	, ("Ptr#",		TPtrU)
	
	, ("Bool",		TBool  Boxed)
	, ("Bool#",		TBool  Unboxed)

	, ("Word",		TWord Boxed)
	, ("Word64",		TWord  (BoxedBits 64))
	, ("Word32",		TWord  (BoxedBits 32))
	, ("Word16",		TWord  (BoxedBits 16))
	, ("Word8",		TWord  (BoxedBits 8))

	, ("Word#",		TWord Unboxed)
	, ("Word64#",		TWord  (UnboxedBits 64))
	, ("Word32#",		TWord  (UnboxedBits 32))
	, ("Word16#",		TWord  (UnboxedBits 16))
	, ("Word8#",		TWord  (UnboxedBits 8))

	, ("Int",		TInt   Boxed)
	, ("Int64",		TInt   (BoxedBits 64))
	, ("Int32",		TInt   (BoxedBits 32))
	, ("Int16",		TInt   (BoxedBits 16))
	, ("Int8",		TInt   (BoxedBits 8))

	, ("Int#",		TInt   Unboxed)
	, ("Int64#",		TInt   (UnboxedBits 64))
	, ("Int32#",		TInt   (UnboxedBits 32))
	, ("Int16#",		TInt   (UnboxedBits 16))
	, ("Int8#",		TInt   (UnboxedBits 8))
	
	, ("Float",		TFloat Boxed)
	, ("Float64",		TFloat (BoxedBits 64))
	, ("Float32",		TFloat (BoxedBits 32))

	, ("Float#",		TFloat Unboxed)
	, ("Float64#",		TFloat (UnboxedBits 64))
	, ("Float32#",		TFloat (UnboxedBits 32))
 
	, ("Char",		TChar  Boxed)
	, ("Char32",		TChar  (BoxedBits 32))

	, ("Char#",		TChar  Unboxed)
	, ("Char32#",		TChar (UnboxedBits 32))
	
	, ("String",		TString Boxed)
	, ("String#",		TString Unboxed) 
	
	, ("Unit",		TUnit)
	, ("Ref",		TRef)
	, ("List",		TList)]



renamePrimVar_class :: String -> Maybe PrimId
renamePrimVar_class ss
	| Just xx	<- splitPrefix "Shape" ss
	= Just (FShape (read xx))
	
	| otherwise
	= Map.lookup ss renamePrimVar_classes
	
renamePrimVar_classes
 	= Map.fromList 
 	[ ("Const",		FConst)
	, ("ConstT",		FConstT)
	, ("Mutable",		FMutable)
	, ("MutableT",		FMutableT)
	, ("Direct",		FDirect)
	, ("DirectT",		FDirectT)
	, ("Lazy",		FLazy)
	, ("LazyT",		FLazyT)
	, ("LazyH",		FLazyH)
	, ("Pure",		FPure)
	, ("Empty",		FEmpty) ]
		
---------------------------------------------------------------------------------------------------
varOfPrimIdWithDataFormat :: PrimId -> Maybe Var
varOfPrimIdWithDataFormat pid
 = case pid of
	TBool   fmt	-> Just $ primTBool   fmt
	TWord   fmt	-> Just $ primTWord   fmt
	TInt    fmt	-> Just $ primTInt    fmt
	TFloat  fmt	-> Just $ primTFloat  fmt
	TChar   fmt	-> Just $ primTChar   fmt
	TString fmt	-> Just $ primTString fmt
	_		-> Nothing








