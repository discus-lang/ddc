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
	, equivTT)
where
import DDC.Type.Exp
import DDC.Type.Pretty		()


-- Equiv ------------------------------------------------------------------------------------------
data Equiv
	= IsEquiv
	| NotEquiv Type Type
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
	NotEquiv{}	-> e1


-- equivTT ----------------------------------------------------------------------------------------
-- | Check if two types are equivalent
equivTT :: Type -> Type -> Equiv
equivTT	t1 t2
	| TNil		<- t1
	, TNil		<- t2
	= IsEquiv

	| TVar v1 _	<- t1
	, TVar v2 _	<- t2
	, v1 == v2
	= IsEquiv

	| TCon tc1	<- t1
	, TCon tc2	<- t2
	, tc1 == tc2
	= IsEquiv

	| TApp t11 t12	<- t1
	, TApp t21 t22	<- t2
	= joinEquiv (equivTT t11 t21) (equivTT t12 t22)

	| otherwise
	= NotEquiv t1 t2



	