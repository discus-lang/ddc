{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Primitive, unboxed types.
module DDC.Base.PrimType
	( Width		(..)
	, PrimType	(..))
where
import DDC.Main.Pretty

-- | Width, in bits, of a primitive value.
data Width
	= Width Int
	deriving (Eq, Show, Ord)


instance Pretty Width PMode where
 ppr (Width w)
	= "W" % w


-- | The type of a primitive machine value.
--   TODO: We'll want to add packed vector types to this in the future.
data PrimType
	
	-- | A pointer to something else.
	= PrimPtr	PrimType

	-- | An unsigned integer.
	| PrimTypeWord	Width
	
	-- | A signed integer.
	| PrimTypeInt	Width

	-- | A floating point value.
	| PrimTypeFloat	Width
	deriving (Eq, Show)


instance Pretty PrimType PMode where
 ppr pt
  = case pt of
	PrimPtr pt'		-> "Ptr"    %% pprPrimTypeParens pt'
	PrimTypeWord  (Width w)	-> "Word:"  %  w
	PrimTypeInt   (Width w)	-> "Int:"   %  w
	PrimTypeFloat (Width w) -> "Float:" %  w

pprPrimTypeParens pt
 = case pt of
	PrimPtr pt' 		-> parens pt'
	_			-> ppr pt

