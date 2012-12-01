{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | The data format of a literal value.
module DDC.Base.DataFormat
	( DataFormat (..)
	, dataFormatIsBoxed
	, dataFormatIsUnboxed
	, dataFormatBoxedOfUnboxed
	, dataFormatUnboxedOfBoxed)
where
import Data.Hashable

-- | The data format of a literal value.
--   The 'Bits' versions are used for Int32, Int64 etc.
--
--   Note that `DataFormat` doesn't correspond exactly to `PrimType`, because
--   we can have literals which are Unboxed LStrings or (Unboxed 32) LChars 
--   but these aren't primitive to the machine.
--
data DataFormat

	-- | Some generically boxed literal (like for String)
	= Boxed
	
	-- | Some boxed value with a given width (like Word32)
	| BoxedBits	Int

	-- | Some generically unboxed literal (like for String#)
	| Unboxed
	
	-- | Some unboxed value with a given width (like Word32#)
	| UnboxedBits	Int
	deriving (Show, Eq, Ord)


instance Hashable DataFormat where
 {-# INLINE hash #-}
 hash fmt 
  = case fmt of
	Boxed			-> hash (1 :: Int)
	BoxedBits bits		-> hash (2 :: Int) + hash bits
	Unboxed			-> hash (3 :: Int)
	UnboxedBits bits	-> hash (4 :: Int) + hash bits
	

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


-- | Convert a boxed data format to the unboxed version, 
--   or `Nothing` if it isn't.
dataFormatBoxedOfUnboxed :: DataFormat -> Maybe DataFormat
dataFormatBoxedOfUnboxed fmt
 = case fmt of
 	Unboxed			-> Just Boxed
	UnboxedBits bits	-> Just $ BoxedBits bits
	_			-> Nothing


-- | Convert an unboxed data format to the boxed version, 
--   or `Nothing` if it isn't.
dataFormatUnboxedOfBoxed :: DataFormat -> Maybe DataFormat
dataFormatUnboxedOfBoxed fmt
 = case fmt of
 	Boxed			-> Just Unboxed
	BoxedBits bits		-> Just $ UnboxedBits bits
	_			-> Nothing

