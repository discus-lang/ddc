{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Base.Prim.PrimCoerce
	(PrimCoerce(..))
where


-- | Coercion between pointer types.
--   We keep the types abstract instead of using `PrimType` because the latter
--   only contains raw address types, not pointers to typed data.
data PrimCoerce ty

	-- | Coercion between unboxed pointer types.
	--   eg between (Ptr# (String# %r1)) and (Ptr# Word8#)
	--   The arguments give the type of the pointed-to data.
	= PrimCoercePtr	ty ty
	
	-- | Coercion betweeen (Ptr# a) and Addr#
	--   The argument gives the type of the pointed-to data.
	| PrimCoercePtrToAddr ty
	
	-- | Coercion between Addr# and (Ptr# a)
	--   The argument gives the type of the pointed-to data.
	| PrimCoerceAddrToPtr ty
	deriving (Eq, Show)
