
-- | The data format of a primitive value.
module DDC.Base.DataFormat
	( DataFormat (..)
	, dataFormatIsBoxed
	, dataFormatIsUnboxed
	, dataFormatBoxedOfUnboxed
	, dataFormatUnboxedOfBoxed)
where


-- | The data format of a primitive value.
--	The 'Bits' versions are used for Int32, Int64 etc.
data DataFormat
	= Boxed
	| BoxedBits	Int
	| Unboxed
	| UnboxedBits	Int
	deriving (Show, Eq, Ord)


-- | Check whether this data format corresponds to an unboxed value
dataFormatIsUnboxed :: DataFormat -> Bool
dataFormatIsUnboxed fmt
 = case fmt of
 	Unboxed		-> True
	UnboxedBits _	-> True
	_		-> False


-- | Check whether this data format corresponds to a boxed value
dataFormatIsBoxed :: DataFormat -> Bool
dataFormatIsBoxed fmt
 = case fmt of
 	Boxed		-> True
	BoxedBits _	-> True
	_		-> False


-- | Convert a boxed data format to the unboxed version
dataFormatBoxedOfUnboxed :: DataFormat -> Maybe DataFormat
dataFormatBoxedOfUnboxed fmt
 = case fmt of
 	Unboxed			-> Just Boxed
	UnboxedBits bits	-> Just $ BoxedBits bits
	_			-> Nothing


-- | Convert an unboxed data format to the boxed version
dataFormatUnboxedOfBoxed :: DataFormat -> Maybe DataFormat
dataFormatUnboxedOfBoxed fmt
 = case fmt of
 	Boxed			-> Just Unboxed
	BoxedBits bits		-> Just $ UnboxedBits bits
	_			-> Nothing

