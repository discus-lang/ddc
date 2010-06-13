
-- | Type equivalence checker.
--	Because we use lists of effects in our type expressions, we can't just use
--	Haskell's baked in (==) operator to test if two types are the same.
--	For example, the following types are equivalent, even though the effects
--	appear in a different order in the list.
--	
--	@
--		(a -(!{Read %r1; Read %r2}) b)	
--		(a -(!{Read %r2; Read %r1}) b)
--	@
--	
module DDC.Type.Equiv
	(equivTT)
where
-- import DDC.Type.Exp


equivTT	= undefined
	