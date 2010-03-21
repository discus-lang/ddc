
module Shared.Base
	( SourcePos  (..) 
	, DataFormat (..)
	, dataFormatIsBoxed
	, dataFormatIsUnboxed
	, dataFormatBoxedOfUnboxed
	, dataFormatUnboxedOfBoxed)
where
import Shared.Pretty


-- | A position in a source file
data SourcePos		
	= SourcePos 
		( String	-- path to file
		, Int		-- line number
		, Int)		-- column number
	deriving (Show, Eq)
	
instance Pretty SourcePos PMode where
 ppr (SourcePos (f, l, c))	= ppr $ f ++ ":" ++ show l ++ ":" ++ show (c - 1)


-- | The data format for a primitive value
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

