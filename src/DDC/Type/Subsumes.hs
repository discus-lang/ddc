{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Subsumption checking.
--
--   TODO: We assume effects and closures are always covariant.
--         The fact that we're not swapping the variance for function 
---	   should not matter due to the form of constraints on higher
--         order effects and closures, but we should check that this
--         form holds during the subsumption rules..
--
module DDC.Type.Subsumes
	( Subsumes(..)
	, isSubsumes
	, joinSubsumes
	, subsumesTT)
where
import DDC.Type.Exp
import DDC.Type.Equiv
import DDC.Type.Kind
import DDC.Type.Compounds

-- Subsumes ---------------------------------------------------------------------------------------
data Subsumes
	= Subsumes
	| NoSubsumes Type Type
	deriving (Show, Eq)

isSubsumes :: Subsumes -> Bool
isSubsumes eq
 = case eq of
	Subsumes	-> True
	_		-> False

joinSubsumes :: Subsumes -> Subsumes -> Subsumes
joinSubsumes e1 e2
 = case e1 of
	Subsumes	-> e2
	NoSubsumes{}	-> e1


-- Type Subsumption -------------------------------------------------------------------------------
-- | Check that the first type is subsumed by the second. @t1 <: t2@
subsumesTT :: Type -> Type -> Subsumes
subsumesTT t1 t2
	| TNil		<- t1
	, TNil		<- t2
	= Subsumes

	-- Constrain -------------------
	-- These might contain more-than constraints that we'll need to check
	-- the subsumption, but they'll also be directly attached to the variables.
	| TConstrain t1' _ <- t1
	= subsumesTT t1' t2
	
	| TConstrain t2' _ <- t2
	= subsumesTT t1 t2'

	-- Forall ---------------------
	| TForall b1 k1 t11	<- t1
	, TForall b2 k2 t21	<- t2
	, b1 == b2
	, isEquiv (equivKK k1  k2)
	, isEquiv (equivTT t11 t21)
	= Subsumes

	-- Sums -----------------------
	-- NOTE: These have to come before the cases for Vars.
	--       When checking: { x1 : %r1 ; x2 : $c1 } <: ($c1 :> (x : %r1))
	--       On the right, we need both the variable $c1 and its more-than bound
	--       to subsume the sum. However, the rules for TVar split the variable
	--       from the bound and try the subsumptions one at a time.
	--      
	| TSum k1 []		<- t1
	, TSum k2 []		<- t2
	, isRegionKind k1 || isEffectKind k1 || isClosureKind k1
	, isEquiv (equivKK k1 k2)
	= Subsumes

	| TSum k1 ts1		<- t1
	, isRegionKind k1 || isEffectKind k1 || isClosureKind k1
	, and $ map (\t1i -> isSubsumes $ subsumesTT t1i t2)  ts1
	= Subsumes
		
	| TSum k2 ts2		<- t2
	, isRegionKind k2 || isEffectKind k2 || isClosureKind k2
	, or $ map (\t2i -> isSubsumes $ subsumesTT t1  t2i) ts2
	= Subsumes

	-- Vars -----------------------
	-- NOTE: These have to come after the cases for sums, as described above.
	| TVar _ b1		<- t1
	, TVar _ b2		<- t2
	, takeVarOfBound b1 == takeVarOfBound b2
	= Subsumes

	| TVar k (UMore v _)	<- t2
	, isSubsumes $ subsumesTT t1 (TVar k (UVar v))
	= Subsumes
	
	| TVar _ (UMore _ t2')	<- t2
 	= subsumesTT t1 t2'
		
	-- Applications ---------------
	-- NOTE: This special case only applies to closures.
	--       Needed for test/15-Typing/InfiniteTypes/Loop1/Loop1.ds
	-- 	 (x : $c1) <: $c1
	-- 
	| Just (_, TVar k1 b1)	<- takeTFree t1
	, TVar k2 b2		<- t2
	, v1			<- takeVarOfBound b1
	, v2			<- takeVarOfBound b2
	, isClosureKind k1
	, isClosureKind k2
	, v1 == v2
	= Subsumes

	| TApp t11 t12	<- t1
	, TApp t21 t22	<- t2
	= joinSubsumes (subsumesTT t11 t21) (subsumesTT t12 t22)

	-- Constructors ---------------
	-- Ignore tag variables when checking whether closure constructors are equivalent.
	| TCon (TyConClosure tcc1 _)	<- t1
	, TCon (TyConClosure tcc2 _)	<- t2
	= case (tcc1, tcc2) of
		(TyConClosureFreeType   _, TyConClosureFreeType _)	-> Subsumes
		(TyConClosureFreeRegion _, TyConClosureFreeRegion _)	-> Subsumes
		(TyConClosureFree _,	   TyConClosureFree _)		-> Subsumes
		_							-> NoSubsumes t1 t2

	| TCon tc1	<- t1
	, TCon tc2	<- t2
	, tc1 == tc2
	= Subsumes

	-- No dice --------------------
	| otherwise
	= NoSubsumes t1 t2

