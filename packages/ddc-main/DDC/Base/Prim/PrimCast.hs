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
	( PrimCast(..)
	, readPrimCast
	, isSimplePrimCast)
where
import DDC.Base.Prim.PrimType
import Data.List


-- | Casting between different numeric types.
data PrimCast
	= PrimCast PrimType PrimType
	deriving (Eq, Show)


-- | Primitive cast functions have names like "primCast_Word8_Float32"
--   Not all combinations of types are accepted.
--   You can't cast anything to or from `Addr` because we want to keep
--   it abstract, and we don't know it's width.
--   See "isSimplePrimCast" for details.
readPrimCast :: String -> Maybe PrimCast
readPrimCast str
	| Just strTypes			<- stripPrefix "primCast_" str
	, (strType1, _ : strType2)	<- break (== '_') strTypes
	, Just pt1			<- readPrimType strType1
	, Just pt2			<- readPrimType strType2
	, isSimplePrimCast $ PrimCast pt1 pt2
	= Just $ PrimCast pt1 pt2
	
	| otherwise
	= Nothing


-- | We allow direct casts
--	between all widths of the same type, 
--  	between types of the same width.
--
--   We disallow cross-casts between different types of different widths
--   so that the backend's job is easier.
--
isSimplePrimCast :: PrimCast -> Bool
isSimplePrimCast (PrimCast pt1 pt2)
	-- Allow casts between all widths of the same type.
	| pt1 /= PrimTypeAddr
	, primTypesHaveSameShape pt1 pt2
	= True
	
	-- Allow casts between different types of the same width.
	| Just width1		<- takeWidthOfPrimType pt1
	, Just width2		<- takeWidthOfPrimType pt2
	, width1 == width2
	= True

	| otherwise
	= False


