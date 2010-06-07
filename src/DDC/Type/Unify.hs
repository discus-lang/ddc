{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Type.Unify
	(unifyTT)
where
import DDC.Type.Exp
import DDC.Type.Kind
import Data.Sequence			(Seq)
import qualified Data.Sequence 		as Seq

-- | Unify two types, if possible,
--	returning a list of type constriants arising due to the unification.
unifyTT :: Type -> Type -> Maybe (Seq (Type, Type))
unifyTT t1 t2

	-- Effects always unify. 
	-- Just return a constraint for these.
	| isEffect t1
	, isEffect t2
	= Just (Seq.singleton (t1, t2))

	-- Closures always unify. 
	-- Just return a constraint for these.
	| isClosure t1
	, isClosure t2
	= Just (Seq.singleton (t1, t2))

	-- applications.
	| TApp t11 t12	<- t1
	, TApp t21 t22	<- t2
	, Just subA	<- unifyTT t11 t21
	, Just subB	<- unifyTT t12 t22
	= Just (subA Seq.>< subB)

	-- constructors.
	| TCon tc1	<- t1
	, TCon tc2	<- t2
	, tc1 == tc2
	= Just Seq.empty
	
	-- same variable.
	| TVar k1 v1	<- t1
	, TVar k2 v2	<- t2
	, k1 == k2
	, v1 == v2		
	= Just Seq.empty
	
	-- variables match anything.
	| TVar k1 _	<- t1
	, k2		<- kindOfType t2
	, k1 == k2	
	= Just $ Seq.singleton (t1, t2)
	
	| TVar k2 _	<- t2
	, k1		<- kindOfType t1
	, k1 == k2
	= Just $ Seq.singleton (t1, t2)

	-- Summations.
	-- We just return a constraint for these.
	-- Let the caller decide how to handle it.
	| TSum k1 _		<- t1
	, k1 == kindOfType t2
	= Just $ Seq.singleton (t1, t2)

	| TSum k2 _		<- t2
	, kindOfType t1 == k2
	= Just $ Seq.singleton (t1, t2)

	| otherwise	
	= Nothing		


