{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Reading source-level names of primitive casting functions.
module DDC.Base.Prim.PrimCast
	( readPrimCast
	, isSafeCast 
	, readCastableType)
where
import DDC.Base.Prim.PrimType
import Data.List


-- | Primitive casting functions have names like "primCast_PtrWord8_PtrFloat32"
--   Not all combinations of types are accepted.
--   See "isSafeCast" for details.
readPrimCast :: String -> Maybe (PrimType, PrimType)
readPrimCast str
	| Just strTypes			<- stripPrefix "primCast_" str
	, (strType1, _ : strType2)	<- break (== '_') strTypes
	, Just pt1			<- readCastableType strType1
	, Just pt2			<- readCastableType strType2
	, isSafeCast pt1 pt2
	= Just (pt1, pt2)
	
	| otherwise
	= Nothing


-- | Read the name of a `PrimType` that can be used in a cast.
--   These are the numeric types, and (single) pointers to them.
readCastableType :: String -> Maybe PrimType
readCastableType str
 	| Just strPtr	<- stripPrefix "PtrU" str
	, Just ptNum	<- readCastableTypeNum strPtr
	= Just $ PrimTypePtr ptNum
	
	| otherwise
	= readCastableTypeNum str

readCastableTypeNum str
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
--	between pointers to numeric types of the same width.
--
--   Casting between pointers to types of different sizes can lead to
--   Bus Errors on machines that can't do misaligned accesses (like PPC and SPARC).
-- 
--   We disallow cross-casts between different numeric types of different widths
--   to make the back-ends job easier. 
--
isSafeCast :: PrimType -> PrimType -> Bool
isSafeCast pt1 pt2
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

	-- Allow casts between pointers to numeric types of the same width.
	| PrimTypePtr pt1'	<- pt1
	, PrimTypePtr pt2'	<- pt2
	, isPrimTypeNum pt1'
	, isPrimTypeNum pt2'
	, Just width1		<- takeWidthOfPrimType pt1'
	, Just width2		<- takeWidthOfPrimType pt2'
	, width1 == width2
	= True
		
	| otherwise
	= False


