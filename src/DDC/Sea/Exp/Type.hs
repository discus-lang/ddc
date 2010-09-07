{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Sea.Exp.Type
	( Type		(..)
	, ObjType	(..))
where
import DDC.Var

-- | Sea types.
--	By the time we've reached the Sea language we only care about operational information.
--	We need to distinguish between boxed and unboxed values, but not much else.
data Type
	-- | The void type.
	= TVoid

	-- | The function type.
	--	These are always first order, so the first parameter will not be another TFun.
	| TFun Type Type

	-- | An unboxed pointer to something else.
	| TPtr Type

	-- | An unboxed data object.
	| TCon Var [Type]

	-- | Some anonymous boxed object. Usually accessed as a pointer.
	| TObj
	deriving (Show, Eq)


-- | When we access fields in an object we need to know exactly what type
--	we are dealing with.
data ObjType
	= TObjData
	| TObjThunk
	| TObjSusp
	deriving (Show, Eq)
