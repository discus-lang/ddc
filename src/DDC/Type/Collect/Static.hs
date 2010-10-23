{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Collection of static variables in a type.
--   TODO: Also collect dangerous variables here.
--	   Once we check our sigs for dangerous variables it will be easy
--         to add tests that check the valididity of closure trimming.
--
module DDC.Type.Collect.Static
	( staticRsT
	, staticRsDataT
	, staticRsClosureT)
where
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Builtin
import Data.Set			(Set)
import qualified Data.Set	as Set

-- | Collect the set of regions that are non-generalisable in a type.
--   This function uses the materiality information present in the data
--   definitions attached to TyCons. If you want to compute the material
--   variables of a type directly then use "DDC.Type.Data.Material"
--
--   We return a set of types so that it can contain meta-variables as well.
--
staticRsT  :: Type -> Set Type
staticRsT tt
 	= Set.union (staticRsDataT tt) (staticRsClosureT tt)


-- | Get the set of regions that are non-generalisable because they are
--   material in this type. 
staticRsDataT :: Type -> Set Type
staticRsDataT tt
 = case tt of
	TNil			-> Set.empty

	TVar k _
	 | k == kRegion		-> Set.singleton tt
	 | otherwise		-> Set.empty

	TSum k ts
	 | k == kEffect		-> Set.empty
	 | k == kClosure	-> Set.unions $ map staticRsDataT ts

	-- TODO: we're taking all args to be material, 
	--	which is a safe overapproximation. 
	TApp{}
	 | Just (_, _, ts)	<- takeTData tt
	 -> Set.unions $ map staticRsDataT ts
	 
	 | Just (_, t)		<- takeTFree tt
	 -> staticRsDataT t
	
	 | Just (t1, t2)	<- takeTDanger tt
	 -> Set.unions $ map staticRsDataT [t1, t2]
	
	 | otherwise		-> Set.empty

	TConstrain t _		-> staticRsDataT t
	
	TForall _ _ t		-> staticRsDataT t	
	TError{}		-> Set.empty
	
	TCon{}			-> Set.empty
	TSum _ ts	
	 -> Set.unions $ map staticRsDataT ts
	
		

-- | Region cids that are free in the closure of the outer-most function
--	constructor(s) are being shared with the caller. These functions
--	did not allocate those regions, so they be can't generalised here.
staticRsClosureT :: Type -> Set Type
staticRsClosureT tt
 = case tt of
	TConstrain t _		-> staticRsClosureT t

	TApp{} 
	 | Just (_, _, _, clo)	<- takeTFun tt
	 -> staticRsDataT clo

	 | Just (_, t2)		<- takeTFree tt
	 -> staticRsDataT t2
	
	 | Just (t1, t2)	<- takeTDanger tt
	 -> Set.unions $ map staticRsDataT [t1, t2]

	 -- TODO: we're taking all args to be material, 
	 --	which is a safe over approximation.
	 | Just (_, _, ts)	<- takeTData tt
	 -> Set.unions $ map staticRsClosureT ts

	TSum k ts
	 | k == kClosure
	 -> Set.unions $ map staticRsClosureT ts

	_ 	-> Set.empty
