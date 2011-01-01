{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Reading source-level names of primitive cast functions.
--
--   Casting between primitive types, such as Int32 and Float32 requires
--   a representation change, and may also require the value to be copied 
--   into a different register.
-- 
--   In contrast, a coersion does not require a representation change, we 
--   just start treating a value as having a different type. In general 
--   this only works for pointers.
--
module DDC.Base.Prim.PrimCast
	( readPrimCast
	, readCastableType
	, isSimpleCast)
where
import DDC.Base.Prim.PrimType
import Data.List


-- | Primitive cast functions have names like "primCast_PtrWord8_PtrFloat32"
--   Not all combinations of types are accepted.
--   See "isSimpleCast" for details.
readPrimCast :: String -> Maybe (PrimType, PrimType)
readPrimCast str
	| Just strTypes			<- stripPrefix "primCast_" str
	, (strType1, _ : strType2)	<- break (== '_') strTypes
	, Just pt1			<- readCastableType strType1
	, Just pt2			<- readCastableType strType2
	, isSimpleCast pt1 pt2
	= Just (pt1, pt2)
	
	| otherwise
	= Nothing


-- | Read the name of a `PrimType` that can be used in a cast.
--   These are the numeric types only.
--   Pointers can't be casted (but they can be coerced).
-- 
readCastableType :: String -> Maybe PrimType
readCastableType str
 = case str of
	"Word8U"	-> Just $ PrimTypeWord  $ Width 8
	"Word16U"	-> Just $ PrimTypeWord  $ Width 16
	"Word32U"	-> Just $ PrimTypeWord  $ Width 32
	"Word64U"	-> Just $ PrimTypeWord  $ Width 64

	"Int32U"	-> Just $ PrimTypeInt   $ Width 32
	"Int64U"	-> Just $ PrimTypeInt   $ Width 64

	"Float32U"	-> Just $ PrimTypeFloat $ Width 32
	"Float64U"	-> Just $ PrimTypeFloat $ Width 64
	_		-> Nothing
	

-- | We allow casts 
--	between all widths of the same numeric type, 
--  	between numeric types of the same width,
--
--   We disallow cross-casts between different numeric types of different widths
--   so that the backend's job is easier.
--
isSimpleCast :: PrimType -> PrimType -> Bool
isSimpleCast pt1 pt2
	-- Allow casts between all widths of the same type.
	| isPrimTypeNum pt1
	, primTypesHaveSameShape pt1 pt2
	= True
	
	-- Allow casts between different numeric types of the same width.
	| isPrimTypeNum pt1
	, isPrimTypeNum pt2
	, Just width1		<- takeWidthOfPrimType pt1
	, Just width2		<- takeWidthOfPrimType pt2
	, width1 == width2
	= True

	| otherwise
	= False


