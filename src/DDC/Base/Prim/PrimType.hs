{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Primitive unboxed types.
module DDC.Base.Prim.PrimType
	( Width		(..)
	, PrimType	(..)
	, takeWidthOfPrimType
	, primTypesHaveSameShape
	, readPrimType)
where
import DDC.Main.Pretty


-- | Width, in bits, of a primitive value.
data Width
	= Width Int
	deriving (Eq, Show, Ord)


-- | Primitive unboxed types that are probably implemented directly by the
--   hardware. Values of different `PrimTypes` may be stored in different 
--   registers, and/or require explicit coersion instructions to convert
--   between them.
--
data PrimType
	
	-- | An address with enough precision to access any byte in the process.
	--   We don't call this a pointer, because it might not point to well
	--   formed data (eg it might be zero).
	= PrimTypeAddr

	-- | An unsigned integer.
	| PrimTypeWord	Width
	
	-- | A signed integer.
	| PrimTypeInt	Width

	-- | A floating point value.
	| PrimTypeFloat	Width
	deriving (Eq, Show)


-- | Take the width of a numeric `PrimType`.
--   `PrimTypeAddr` don't have a width.
takeWidthOfPrimType :: PrimType -> Maybe Width
takeWidthOfPrimType pt
 = case pt of
	PrimTypeAddr{}	-> Nothing
	PrimTypeWord  w	-> Just w
	PrimTypeInt   w	-> Just w
	PrimTypeFloat w	-> Just w


-- | Check if two `PrimType`s are similar, ignoring the `Width` fields.
primTypesHaveSameShape :: PrimType -> PrimType -> Bool
primTypesHaveSameShape pt1 pt2
 = case (pt1, pt2) of
	(PrimTypeAddr,    PrimTypeAddr)		-> True
	(PrimTypeWord{},  PrimTypeWord{})	-> True
	(PrimTypeInt{},	  PrimTypeInt{})	-> True
	(PrimTypeFloat{}, PrimTypeFloat{})	-> True
	_ 					-> False
	

-- Read -------------------------------------------------------------------------------------------	
-- | Read the module name of an unboxed `PrimType`, eg "Word32U" (ending in U).
--   This only works for widths that are supported by the runtime system and backends.
readPrimType :: String -> Maybe PrimType
readPrimType str
 = case str of
	"AddrU"		-> Just $ PrimTypeAddr

	"Word8U"	-> Just $ PrimTypeWord  $ Width 8
	"Word16U"	-> Just $ PrimTypeWord  $ Width 16
	"Word32U"	-> Just $ PrimTypeWord  $ Width 32
	"Word64U"	-> Just $ PrimTypeWord  $ Width 64

	"Int8U"		-> Just $ PrimTypeInt   $ Width 8
	"Int16U"	-> Just $ PrimTypeInt   $ Width 16
	"Int32U"	-> Just $ PrimTypeInt   $ Width 32
	"Int64U"	-> Just $ PrimTypeInt   $ Width 64

	"Float32U"	-> Just $ PrimTypeFloat $ Width 32
	"Float64U"	-> Just $ PrimTypeFloat $ Width 64
	_		-> Nothing
	

-- Pretty -----------------------------------------------------------------------------------------
instance Pretty Width PMode where
 ppr (Width w)
	= "W" % w

instance Pretty PrimType PMode where
 ppr pt
  = case pt of
	PrimTypeAddr		-> ppr "AddrU"
	PrimTypeWord  (Width w)	-> "Word"   %  w % "U"
	PrimTypeInt   (Width w)	-> "Int"    %  w % "U"
	PrimTypeFloat (Width w) -> "Float"  %  w % "U"
