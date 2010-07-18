{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Type equivalence checker.
--	Because we use lists of effects in our type expressions, we can't just use
--	Haskell's baked in (==) operator to test if two types are the same.
--	For example, the following types are equivalent, even though the effects
--	appear in a different order in the list.
--	
--	@
--	  (a -(!{Read %r1; Read %r2}) b)	
--	  (a -(!{Read %r2; Read %r1}) b)
--	@
--	
module DDC.Type.Equiv
	( Equiv (..)
	, isEquiv
	, equivTT
	, equivKK)
where
import DDC.Type.Exp
import DDC.Type.Kind
import DDC.Type.Pretty		()


-- Equiv ------------------------------------------------------------------------------------------
data Equiv
	= IsEquiv
	| NotEquivTypes Type Type
	| NotEquivKinds Kind Kind
	deriving (Show, Eq)

isEquiv :: Equiv -> Bool
isEquiv eq
 = case eq of
	IsEquiv	-> True
	_	-> False

joinEquiv :: Equiv -> Equiv -> Equiv
joinEquiv e1 e2
 = case e1 of
	IsEquiv		-> e2
	NotEquivTypes{}	-> e1
	NotEquivKinds{}	-> e1


-- Type Equivalence -------------------------------------------------------------------------------
-- | Check if two types are equivalent 
--   TODO: use equivKK instead of equality via kindOfType, too slow and maybe wrong with types in kinds.
equivTT :: Type -> Type -> Equiv
equivTT	t1 t2
	| TNil		<- t1
	, TNil		<- t2
	= IsEquiv

	| TVar v1 _	<- t1
	, TVar v2 _	<- t2
	, v1 == v2
	= IsEquiv

	-- Ignore tag variables when checking whether closure constructors are equivalent.
	| TCon (TyConClosure tcc1 _)	<- t1
	, TCon (TyConClosure tcc2 _)	<- t2
	= case (tcc1, tcc2) of
		(TyConClosureFreeType   _, TyConClosureFreeType _)	-> IsEquiv
		(TyConClosureFreeRegion _, TyConClosureFreeRegion _)	-> IsEquiv
		(TyConClosureFree _,	   TyConClosureFree _)		-> IsEquiv
		_							-> NotEquivTypes t1 t2

	| TCon tc1	<- t1
	, TCon tc2	<- t2
	, tc1 == tc2
	= IsEquiv

	| TSum k1 []	<- t1
	, TSum k2 []	<- t2
	, isEquiv $ equivKK k1 k2		
	= IsEquiv

	| TSum k1 ts1	<- t1
	, TSum k2 ts2	<- t2
	, isEquiv $ equivKK k1 k2
	, or $ map (isEquiv . equivTT t1) ts2
	, or $ map (isEquiv . equivTT t2) ts1
	= IsEquiv

	| TSum k1 ts1	<- t1
	, isEquiv $ equivKK (kindOfType t2) k1
	, or $ map (isEquiv . equivTT t2) ts1
	= IsEquiv
	
	| TSum k2 ts2	<- t2
	, isEquiv $ equivKK (kindOfType t1) k2
	, or $ map (isEquiv . equivTT t1) ts2
	= IsEquiv

	| TApp t11 t12	<- t1
	, TApp t21 t22	<- t2
	= joinEquiv (equivTT t11 t21) (equivTT t12 t22)

	| otherwise
	= NotEquivTypes t1 t2


-- Kind Equivalence -------------------------------------------------------------------------------
-- | Check if two kinds are equivalent
equivKK :: Kind -> Kind -> Equiv
equivKK k1 k2
	| KNil		<- k1
	, KNil		<- k2
	= IsEquiv
		
	| KCon kc1 _ 	<- k1
	, KCon kc2 _ 	<- k2
	, kc1 == kc2
	= IsEquiv
	
	| KFun k11 k12	<- k1
	, KFun k21 k22	<- k2
	= joinEquiv (equivKK k11 k21) (equivKK k12 k22)
	
	| KApp k11 t12	<- k1
	, KApp k21 t22	<- k2
	= joinEquiv (equivKK k11 k21) (equivTT t12 t22)
	
	| KSum []	<- k1
	, KSum []	<- k2
	= IsEquiv
	
	| KSum ks1	<- k1
	, KSum ks2	<- k2
	, or $ map (isEquiv . equivKK k1) ks2
	, or $ map (isEquiv . equivKK k2) ks1
	= IsEquiv
	
	| KSum ks1	<- k1
	, or $ map (isEquiv . equivKK k2) ks1
	= IsEquiv
	
	| KSum ks2	<- k2
	, or $ map (isEquiv . equivKK k1) ks2
	= IsEquiv
	
	| otherwise
	= NotEquivKinds k1 k2

