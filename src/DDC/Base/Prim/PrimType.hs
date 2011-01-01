{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Primitive unboxed types.
module DDC.Base.Prim.PrimType
	( Width		(..)
	, PrimType	(..)
	, isPrimTypePtr
	, isPrimTypeNum
	, takeWidthOfPrimType
	, primTypesHaveSameShape)
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
	
	-- | A pointer to something else.
	= PrimTypePtr

	-- | An unsigned integer.
	| PrimTypeWord	Width
	
	-- | A signed integer.
	| PrimTypeInt	Width

	-- | A floating point value.
	| PrimTypeFloat	Width
	deriving (Eq, Show)


-- | Check if a `PrimType` is a `Ptr`.
isPrimTypePtr :: PrimType -> Bool
isPrimTypePtr pt
 = case pt of
	PrimTypePtr{}	-> True
	_		-> False


-- | Check if a `PrimType` is a `Word`, `Int` or `Float`.
isPrimTypeNum :: PrimType -> Bool
isPrimTypeNum pt
 = case pt of
	PrimTypeWord{}	-> True
	PrimTypeInt{}	-> True
	PrimTypeFloat{}	-> True
	_		-> False


-- | Take the width of a numeric `PrimType`.
--   Returns `Nothing` if it's a `PrimTypePtr`.
takeWidthOfPrimType :: PrimType -> Maybe Width
takeWidthOfPrimType pt
 = case pt of
	PrimTypePtr{}	-> Nothing
	PrimTypeWord  w	-> Just w
	PrimTypeInt   w	-> Just w
	PrimTypeFloat w	-> Just w


-- | Check if two `PrimType`s are similar, ignoring the `Width` fields.
primTypesHaveSameShape :: PrimType -> PrimType -> Bool
primTypesHaveSameShape pt1 pt2
 = case (pt1, pt2) of
	(PrimTypeWord{},  PrimTypeWord{})	-> True
	(PrimTypeInt{},	  PrimTypeInt{})	-> True
	(PrimTypeFloat{}, PrimTypeFloat{})	-> True
	(PrimTypePtr{},   PrimTypePtr{})	-> True
	_ 					-> False
	

-- Pretty -----------------------------------------------------------------------------------------
instance Pretty Width PMode where
 ppr (Width w)
	= "W" % w

instance Pretty PrimType PMode where
 ppr pt
  = case pt of
	PrimTypePtr		-> ppr "PtrU"
	PrimTypeWord  (Width w)	-> "Word"   %  w % "U"
	PrimTypeInt   (Width w)	-> "Int"    %  w % "U"
	PrimTypeFloat (Width w) -> "Float"  %  w % "U"
