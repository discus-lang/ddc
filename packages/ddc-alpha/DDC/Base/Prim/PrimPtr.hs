
{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Base.Prim.PrimPtr
	(PrimPtr(..))
where
import DDC.Base.Prim.PrimType

-- | Primitive pointer operations.
--   For all of these ops, the parameter gives the type of the pointed-to data.
--   TODO: How to we write PeekOn and PokeOn in terms of the others, while 
--         still retaining the desired types?
data PrimPtr

	-- | Add an offset in bytes to a (Ptr# Word8#)
	= PrimPtrPlus

	-- | Read a value from a pointer.
	| PrimPtrPeek   PrimType

	-- | Read a value from a pointer,
	--   ignoring the first argument of the function.
	| PrimPtrPeekOn PrimType

	-- | Write a value to the pointed-to location.
	| PrimPtrPoke   PrimType

	-- | Write a value to a pointed-to location,
	--   ignoring the first argument of the function.
	| PrimPtrPokeOn PrimType
	deriving (Eq, Show)
	