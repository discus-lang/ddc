
module Shared.VarPrim

where

import Util
import qualified Shared.Var	as Var
import Shared.Var		(NameSpace(..), VarBind(..), Module(..))
import Shared.VarBind

			
-----------------------
-- from Base

-- primitive unboxed types.
primTVoidU	= primVarI NameType	"Base.Void#"	TVoidU		[Var.ISeaName "void"]
primTPtrU	= primVarI NameType	"Base.Ptr#"	TPtrU		[Var.ISeaName "Ptr"]

primTBoolU	= primVarI NameType	"Base.Bool#"	TBoolU		[Var.ISeaName "Bool"]

primTWord8U	= primVarI NameType	"Base.Word8#"	TWord8U		[Var.ISeaName "Word8"]
primTWord16U	= primVarI NameType	"Base.Word16#"	TWord16U	[Var.ISeaName "Word16"]
primTWord32U	= primVarI NameType	"Base.Word32#"	TWord32U	[Var.ISeaName "Word32"]
primTWord64U	= primVarI NameType	"Base.Word64#"	TWord64U	[Var.ISeaName "Word64"]

primTInt8U	= primVarI NameType	"Base.Int8#"	TInt8U		[Var.ISeaName "Int8"]
primTInt16U	= primVarI NameType	"Base.Int16#"	TInt16U		[Var.ISeaName "Int16"]
primTInt32U	= primVarI NameType	"Base.Int32#"	TInt32U		[Var.ISeaName "Int32"]
primTInt64U	= primVarI NameType	"Base.Int64#"	TInt64U		[Var.ISeaName "Int64"]

primTFloat32U	= primVarI NameType	"Base.Float32#"	TFloat32U	[Var.ISeaName "Int32"]
primTFloat64U	= primVarI NameType	"Base.Float64#"	TFloat64U	[Var.ISeaName "Int64"]

primTChar32U	= primVarI NameType	"Base.Char32#"	TChar32U	[Var.ISeaName "Char32"]
primTStringU	= primVarI NameType	"Base.String#"	TStringU	[Var.ISeaName "String"]


-- primitive boxed types
primTBool	= primVar NameType 	"Base.Bool"				TBool

primTWord	= primVar NameType	"Base.Word32"				TWord32
primTWord8	= primVar NameType	"Base.Word8"				TWord8
primTWord16	= primVar NameType	"Base.Word16"				TWord16
primTWord32	= primVar NameType	"Base.Word32"				TWord32
primTWord64	= primVar NameType	"Base.Word64"				TWord64

-- hack Int -> Int32
--	would be better to have real type aliases
primTInt	= primVar NameType	"Base.Int32"				TInt32

primTInt8	= primVar NameType	"Base.Int8"				TInt8
primTInt16	= primVar NameType	"Base.Int16"				TInt16
primTInt32	= primVar NameType	"Base.Int32"				TInt32
primTInt64	= primVar NameType	"Base.Int64"				TInt64

-- hack Float -> Float32
primTFloat	= primVar NameType	"Base.Float32"				TFloat32
primTFloat32	= primVar NameType	"Base.Float32"				TFloat32
primTFloat64	= primVar NameType	"Base.Float64"				TFloat64

-- hack Char -> Char32
primTChar	= primVar NameType	"Base.Char32"				TChar32
primTChar32	= primVar NameType	"Base.Char32"				TChar32

primTString	= primVar NameType	"Base.String"				TString


-- simple types
primTUnit	= primVar NameType	"Base.Unit"				TUnit
primUnit	= primVar NameValue	"Base.Unit"				VUnit



primTObj	= primVar NameType	"Base.Obj"				TObj
primTData	= primVar NameType	"Base.Data"				TData
primTThunk	= primVar NameType	"Base.Thunk"				TThunk

primFShape i	= primVar NameClass	("Base.Shape" ++ show i)		(FShape  i)
primFUnify i	= primVar NameClass	("Base.Unify" ++ show i)		(FUnify  i)
primFInject i	= primVar NameClass	("Base.Inject" ++ show i)		(FInject i)

primProjField	= primVarI NameValue	"Base.primProjField"			VProjField
					[ Var.ISeaName "primProjField"]

primProjFieldR	= primVarI NameValue	"Base.primProjFieldR"			VProjFieldR
					[ Var.ISeaName "primProjFieldR"]


--	effects
primRead	= primVar NameEffect	"Base.Read"				ERead
primReadT	= primVar NameEffect	"Base.ReadT"				EReadT
primReadH	= primVar NameEffect	"Base.ReadH"				EReadH

primWrite	= primVar NameEffect	"Base.Write"				EWrite
primWriteT	= primVar NameEffect	"Base.WriteT"				EWriteT

--	classes
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

-- from Base.Thunk
primSuspend i	= primVarI NameValue	("Base.Thunk.suspend" ++ show i) 	(VSuspend i) 
					[Var.ISeaName ("primSuspend" ++ show i)]

-- from Class.Num
primNegate	= primVar NameValue	"Class.Num.negate"			VNegate
								
-- from Data.Bool
primTrue	= primVar NameValue 	"Data.Bool.True"			VTrue
primFalse	= primVar NameValue	"Data.Bool.False"			VFalse

-- from Data.Array
primIndex	= primVarI NameValue	"Data.Array.index"			VIndex
					[Var.ISeaName "primArray_index"]
					
primIndexR	= primVarI NameValue	"Data.Array.indexR"			VIndexR
					[Var.ISeaName "primArray_indexR"]

-- from Data.Ref
primTRef	= primVar NameType	"Data.Ref.Ref"				TRef


-- from Data.Tuple
primTTuple i	= primVar NameType	("Data.Tuple.Tuple" ++ show i) 		(TTuple i)	
primTuple i	= primVar NameValue	("Data.Tuple.Tuple" ++ show i) 		(VTuple i)


-- from Data.List
primTList	= primVar NameType	"Data.List.List"			TList
primNil		= primVar NameValue	"Data.List.Nil"				VNil	
primCons	= primVar NameValue	"Data.List.Cons"			VCons	


primAppend	= primVar NameValue	"Data.List.Append"			VAppend	

primRange	= primVar NameValue	"Data.List.rangeInt" 			VRange
primRangeL	= primVar NameValue	"Data.List.rangeIntL"			VRangeL
primRangeInfL	= primVar NameValue	"Data.List.rangeInfIntL"		VRangeInfL
primConcatMap	= primVar NameValue	"Data.List.concatMap"			VConcatMap
primConcatMapL	= primVar NameValue	"Data.List.concatMapL"			VConcatMapL


-- from Control.Exception
primThrow	= primVarI NameValue	"Control.Exception.primThrow"		VThrow
					[Var.ISeaName ("primException_throw")]

primTry		= primVarI NameValue	"Control.Exception.primTry"		VTry
					[Var.ISeaName ("primException_try")]

primExceptionBreak
		= primVar NameValue	"Control.Exception.ExceptionBreak"	VExceptionBreak	

-- from Control.Imperative
primGateLoop	= primVar NameValue	"Control.Exception.gateLoop"		VGateLoop

primWhile	= primVar NameValue	"Control.Imperative.while"		VWhile	

	


-----
primVar space name bind
 = let	parts		= breakOns '.' name
 	Just modParts	= takeInit parts
	Just varName	= takeLast parts

   in	(Var.new varName)
 	{ Var.bind		= bind
	, Var.nameSpace		= space
	, Var.nameModule	= ModuleAbsolute modParts }

primVarI space name bind info
  = (primVar space name bind) { Var.info = info }


bindPrimVar n v
 
 | NameValue	<- n
 = case Var.name v of
	"negate"	-> Just $ v { Var.bind = VNegate }
	"Unit"		-> Just $ v { Var.bind = VUnit }
	'T':'u':'p':'l':'e':xs
			-> Just $ v { Var.bind = VTuple (read xs) }

	"True"		-> Just $ v { Var.bind = VTrue }
	"False"		-> Just $ v { Var.bind = VFalse }
	
	's':'u':'s':'p':'e':'n':'d':xs
			-> Just $ v { Var.bind = VSuspend (read xs) }

	"index"		-> Just $ v { Var.bind = VIndex }
	"indexR"	-> Just $ v { Var.bind = VIndexR }

	"primProjField"	-> Just $ v { Var.bind = VProjField }
	"primProjFieldR" -> Just $ v { Var.bind = VProjFieldR }

	
	"Nil"		-> Just $ v { Var.bind = VNil }
	"Cons"		-> Just $ v { Var.bind = VCons }
	"Append"	-> Just $ v { Var.bind = VAppend }

	"ExceptionBreak" 	-> Just $ v { Var.bind = VExceptionBreak }
	"gateLoop"		-> Just $ v { Var.bind = VGateLoop	 }

	"primThrow"	-> Just $ v { Var.bind = VThrow }
	"primTry"	-> Just $ v { Var.bind = VTry }
	
 	"rangeInt"	-> Just $ v { Var.bind = VRange  }
	"rangeIntL"	-> Just $ v { Var.bind = VRangeL }
	"rangeInfIntL"	-> Just $ v { Var.bind = VRangeInfL }

	"concatMap"	-> Just $ v { Var.bind = VConcatMap }
	"concatMapL"	-> Just $ v { Var.bind = VConcatMapL }
	_		-> Nothing

 | NameType	<- n
 = case Var.name v of
 	"Unit"		-> Just $ v { Var.bind = TUnit }
	"Bool"		-> Just $ v { Var.bind = TBool 		}

	-- hack these to 32 bit for now
	"Int"		-> Just $ v { Var.bind = TInt32 	}
	"Float"		-> Just $ v { Var.bind = TFloat32 	}
	"Char"		-> Just $ v { Var.bind = TChar32	}

	"String"	-> Just $ v { Var.bind = TString	}
	"Ref"		-> Just $ v { Var.bind = TRef 		}

	'T':'u':'p':'l':'e':xs
			-> Just $ v { Var.bind = TTuple (read xs) }
	
	"List"		-> Just $ v { Var.bind = TList		}
	
	"Obj"		-> Just $ v { Var.bind = TObj 		}
	"Data"		-> Just $ v { Var.bind = TData 		}
	"Thunk"		-> Just $ v { Var.bind = TThunk 	}
	
	-- primitive types
	"Void#"		-> Just $ v { Var.bind = TVoidU		}
	"Ptr#"		-> Just $ v { Var.bind = TPtrU		}

	"Bool#"		-> Just $ v { Var.bind = TBoolU 	}

	"Word8#"	-> Just $ v { Var.bind = TWord8U	}
	"Word16#"	-> Just $ v { Var.bind = TWord16U	}
	"Word32#"	-> Just $ v { Var.bind = TWord32U	}
	"Word64#"	-> Just $ v { Var.bind = TWord64U	}
	
	"Int8#"		-> Just $ v { Var.bind = TInt8U 	}
	"Int16#"	-> Just $ v { Var.bind = TInt16U 	}
	"Int32#"	-> Just $ v { Var.bind = TInt32U 	}
	"Int64#"	-> Just $ v { Var.bind = TInt64U 	}

	"Float32#"	-> Just $ v { Var.bind = TFloat32U	}
	"Float64#"	-> Just $ v { Var.bind = TFloat64U	}

	"Char32#"	-> Just $ v { Var.bind = TChar32U	}
	"String#"	-> Just $ v { Var.bind = TStringU	}

	---

	_		-> Nothing

	
 | NameEffect	<- n
 = case Var.name v of
 	"Read"		-> Just $ v { Var.bind = ERead }
	"ReadT"		-> Just $ v { Var.bind = EReadT }
	"ReadH"		-> Just $ v { Var.bind = EReadH }


	"Write"		-> Just $ v { Var.bind = EWrite }
	"WriteT"	-> Just $ v { Var.bind = EWriteT }
	_		-> Nothing
	
 | NameClass	<- n
 = case Var.name v of
	"ConstT"	-> Just $ v { Var.bind = FConstT }
	"MutableT"	-> Just $ v { Var.bind = FMutableT }
	"DirectT"	-> Just $ v { Var.bind = FDirectT }

	"LazyT"		-> Just $ v { Var.bind = FLazyT }
	"LazyH"		-> Just $ v { Var.bind = FLazyH }

 	"Lazy"		-> Just $ v { Var.bind = FLazy }
	"Const"		-> Just $ v { Var.bind = FConst }
	"Mutable"	-> Just $ v { Var.bind = FMutable }
	"Direct"	-> Just $ v { Var.bind = FDirect }
	
 	"Pure"		-> Just $ v { Var.bind = FPure }
	"Empty"		-> Just $ v { Var.bind = FEmpty }

	'S':'h':'a':'p':'e':xs
			-> Just $ v { Var.bind = FShape (read xs) }

	'U':'n':'i':'f':'y':xs
			-> Just $ v { Var.bind = FUnify (read xs) }

	'I':'n':'j':'e':'c':'t':xs
			-> Just $ v { Var.bind = FInject (read xs) }

	_		-> Nothing
	
 | otherwise
 =	Nothing
	
	
	
	
	
