
module Core.Util.Subsumes
	(subsumes)

where

import Core.Exp
import Core.Pretty
import Shared.Error
import Util

--
stage	= "Core.Util.Subsumes"

-- | Check if one type subsumes another
--	BUGS: assumes that effect and closures in data types are all covariant
--
subsumes :: Bool -> Type -> Type -> Bool
subsumes topECVars t s
 = let 	?topECVars	= topECVars
	?t		= t
	?s		= s
   in	subsumes' t s
	
subsumes' t s

	-- sums
	| TSum tKind ts		<- t
	, TSum sKind ss		<- s
	, tKind == sKind
	= or $ map (\si -> subsumes' t si) ss
	
	-- sum / single
	| TSum k ts		<- t
	, elem k [KEffect, KClosure]
	= or $ map (\ti -> subsumes' ti s) ts

	-- single / sum
	| TSum k ss		<- s
	, elem k [KEffect, KClosure]
	= and $ map (\si -> subsumes' t si) ss

	
	-- masks
	| TMask k t1 t2		<- t
	, TMask k s1 s2		<- s
	, subsumes' t1 s1
	, t2 == s2
	= True 

	-- var
	| TVar tKind tVar	<- t
	, TVar sKind sVar	<- s
	, tKind == sKind
	, tVar == sVar
	= True

	-- anything subsumes bottom
	| TBot _		<- s
	= True
	
	-- top subsumes everything
	| TTop _		<- t
	= True

	-- fun
 	| TFunEC t1 t2 tEff tClo	<- t
	, TFunEC s1 s2 sEff sClo	<- s
	, subsumes' s1 t1
	, subsumes' t2 s2
	, subsumes' tEff sEff
	, subsumes' tClo sClo
	= True
	
	-- data 
	| TData tVar ts		<- t
	, TData sVar ss		<- s
	, tVar == sVar
	, length ts == length ss
	, and $ zipWith subsumes' ts ss
	= True
	
	-- effect constructor
	| TEffect tVar ts	<- t
	, TEffect sVar ss	<- s
	, tVar == sVar
	, ts == ss
	= True
	
	-- closure constructor
	| TFree tVar ts		<- t
	, TFree sVar ss		<- s
	, tVar == sVar
	, ts == ss
	= True
	
	-- classs
	| TClass tVar ts	<- t
	, TClass sVar ss	<- s
	, tVar == sVar
	, ts == ss
	= True
	
	--
	| otherwise
	= False

	-- If we were told that we're in a contra-variant branch then allow
	--	effect and closure variables to subsume everything
	--	BUGS: 	this is nasty, and wrong
	--		before this is valid we need to prove that there are no Pure constraints
	--		on effects, or if there are - that the effects subsumed don't contain 
{-	| TVar k v		<- t
	, elem k [KEffect, KClosure]
	, ?topECVars
	= warning stage
		("subsumes: did a nasty, unchecked subsumpton. s <: t\n"
		% "   s = " % s	% "\n"
		% "   t = " % t % "\n")
		False
-}	
	-----
{-	| otherwise
	= freakout stage
		 ("subsumes:  S is not <: T\n"
		% "    S = " % s % "\n"
		% "    T = " % t % "\n\n"
		% "    when checking\n"
		% "    s = " % ?s % "\n"
		% "    t = " % ?t % "\n")
		
		$ False
-}

