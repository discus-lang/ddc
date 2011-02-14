{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Converting between `Type` and `PrimType`
module DDC.Type.PrimType
	( takeTypeOfPrimType
	, takePrimTypeOfType
	, takePrimTypeOfPtrType)
where
import DDC.Base.Prim.PrimType
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Builtin
import DDC.Var.PrimId
import DDC.Var
import DDC.Base.DataFormat
import Shared.VarPrim


-- | Take the `Type` of a `PrimType`
takeTypeOfPrimType :: PrimType -> Maybe Type
takeTypeOfPrimType pt
 = case pt of
	PrimTypeWord  (Width w)	-> Just $ makeTData (primTWord  (UnboxedBits w)) kValue []
	PrimTypeInt   (Width w)	-> Just $ makeTData (primTInt   (UnboxedBits w)) kValue []
	PrimTypeFloat (Width w)	-> Just $ makeTData (primTFloat (UnboxedBits w)) kValue []
	_			-> Nothing

-- | Take the `PrimType` of an unboxed `Type`.
takePrimTypeOfType :: Type -> Maybe PrimType
takePrimTypeOfType tt
	| Just (v, _, [])	<- takeTData tt
	= case varId v of
		VarIdPrim (TWord  (UnboxedBits w))	-> Just $ PrimTypeWord  (Width w)
		VarIdPrim (TInt   (UnboxedBits w))	-> Just $ PrimTypeInt   (Width w)
		VarIdPrim (TFloat (UnboxedBits w))	-> Just $ PrimTypeFloat (Width w)
		_					-> Nothing

	| otherwise		= Nothing


-- | For a `Type` like Ptr# Word8# take the element type (Word8#) as a PrimType.
takePrimTypeOfPtrType :: Type -> Maybe PrimType
takePrimTypeOfPtrType tt
 	| Just (v, _, [t])	<- takeTData tt
	, varId v == VarIdPrim TPtrU
	= takePrimTypeOfType t
	
	| otherwise
	= Nothing


