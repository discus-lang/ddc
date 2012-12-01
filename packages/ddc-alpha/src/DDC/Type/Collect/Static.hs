{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Collection of static variables in a type.
--   TODO: Also collect dangerous variables here.
--	   Once we check our sigs for dangerous variables it will be easy
--         to add tests that check the valididity of closure trimming.
--
module DDC.Type.Collect.Static
	( staticTsUnboxedT
	, staticRsT
	, staticRsDataT
	, staticRsClosureT
	, materialRsT)
where
import DDC.Type.Exp
import DDC.Type.Kind
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Builtin
import DDC.Type.Pretty		()
import DDC.Type.Collect.FreeTVars
import DDC.Main.Error
import DDC.Main.Pretty
import Data.Set			(Set)
import qualified Data.Set	as Set
import qualified Data.Map	as Map

stage	= "DDC.Type.Collect.Static"

-- | Collect the set of value type variables that cannot be generalised
--   because they appear in unboxed types (like with Ptr# a).
staticTsUnboxedT  :: Type -> Set Type
staticTsUnboxedT tt
 	| isUnboxedT tt
	= freeTVars tt

	| otherwise
	= Set.empty

-- | Collect the set of regions that are non-generalisable in a type.
--   This function uses the materiality information present in the data
--   definitions attached to TyCons. If you want to compute the material
--   variables of a type directly then use "DDC.Type.Data.Material"
--
--   We return a set of types so that it can contain meta-variables as well.
--  TODO: This is wrong.
--
staticRsT  :: Type -> Set Type
staticRsT tt
 	= Set.union (staticRsDataT tt) (staticRsClosureT tt)


-- | Get the set of regions that are non-generalisable because they are
--   material in this type. 
--   TODO: This is wrong. use freeMateriality utils instead.
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
--  TODO: this is wrong.
staticRsClosureT :: Type -> Set Type
staticRsClosureT tt
 = case tt of
	TConstrain t _
	 -> staticRsClosureT t

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



-- | Compute the material vars in a type
--   TODO: This isn't finished.
--         All vars in a data type are taken to be material.
--         Use freeMaterialityUtils instead.
materialRsT :: Type -> Set Type
materialRsT tt
	= materialRsWithCrs emptyConstraints tt
	
materialRsWithCrs crs tt
 = let down	= materialRsWithCrs crs
   in case tt of
	TNil			-> Set.empty

	TVar k _
	 | isValueKind  k	-> Set.singleton tt
	 | isRegionKind k	-> Set.singleton tt
	 | isEffectKind k	-> Set.empty

	 | isClosureKind k
	 -> case Map.lookup tt (crsEq crs) of
	 	Nothing		-> Set.empty
		Just clo	-> down clo

	 |  isClosureKind k
	 ,  Just t2	<- Map.lookup tt (crsMore crs)
	 -> warning	(vcat	[ stage %% "materialRsT"
				, ppr "Following more-than constraint for" %% tt
				, ppr "Check this is ok"])
	 		$ down t2
	 
	 | otherwise
	 -> warning	(vcat	[ stage %% "materialRsT"
				, ppr "Assuming" %% tt %% "has no material vars"
				, ppr "Check this is ok"])
			$ Set.empty
	
	TCon{}			-> Set.empty
	
	TSum _ ts		-> Set.unions $ map down ts
	
	-- TODO: we're taking all args to be material, 
	--       which is a safe overapproximation if we're just using this
	--       to quantify sigs. Not safe for reducing Shape constraints though.
	TApp{}
	 | Just (_, _, ts)	<- takeTData tt
	 -> Set.unions $ map down ts
	 
	 | Just (_, t)		<- takeTFree tt
	 -> down t

	 | Just (_, _, _, clo)	<- takeTFun tt
	 -> down clo
	
	 | otherwise
	 -> warning 	(vcat	[ stage %% "materialRsT"
				, ppr "Assuming type application" %% tt %% "has no material vars."
				, ppr "Check this is ok." ])
	 		$ Set.empty
	
	TForall{}
	 -> panic stage $ "materialRsT: shouldn't be passed a quantified type."
	
	TConstrain tBody crs'
	 -> materialRsWithCrs (plusConstraints crs crs') tBody
	
	TError{}
	 -> panic stage $ "materialRsT: got TError"


