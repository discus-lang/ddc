
module Core.Util.Subsumes
	(subsumes)

where

import Core.Exp
import Core.Pretty
import Core.ReconKind
import Core.Util.Trim
import Shared.Error
import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Debug.Trace

-----
debug	= False
trace ss x	
 	= if debug 
		then Debug.Trace.trace (pprStr ss) x
		else x

stage	= "Core.Util.Subsumes"
-----

-- | Check if t subsumes s another. t :> s
--	BUGS: assumes that effect and closures in data types are all covariant
--
subsumes 
	:: Map Var Type 	-- table of (v :> t) constraints
	-> Type 
	-> Type 
	-> Bool
subsumes tableMore t s
 = let 	?t		= t
	?s		= s
   in	{-# SCC "subsumes" #-} subsumes1 tableMore t s
	
subsumes1 table t s
 = let ans	= subsumes2 table t s
   in  trace 	("subsumes (T :> S) |- " % ans 	% "\n"
	 	% "    T = " %> t 		% "\n"
		% "    S = " %> s		% "\n")
		ans

-- make sure closure terms are trimmed before comparing them
subsumes2 table t s
 = let trimC tt
 	| kindOfType tt == KClosure	= trimClosureC tt
	| otherwise			= tt
	
   in	subsumes3 table (trimC t) (trimC s)

subsumes3 table t s

	-- load up embedded FMore constraints
	| TFetters t' fs	<- t
	= let table'	= foldl (\tab (FMore v1 t2) -> Map.insert v1 t2 tab) table fs
	  in  subsumes3 table' t' s
	

	| TFetters s' fs	<- s
	= let table'	= foldl (\tab (FMore v1 t2) -> Map.insert v1 t2 tab) table fs
	  in  subsumes3 table' t s'


	-- SubRefl
	| t == s
	= True

	-- SubTop
	-- top subsumes everything
	| TTop _		<- t
	= True

	-- SubBot
	-- anything subsumes bottom
	| TBot _		<- s
	= True

	-- SubVar
	-- G[t :> S] |- S <: t
	| TVar tKind tVar	<- t
	, Just s2		<- Map.lookup tVar table
	, s2 == s
	= True

	-- SubAll
	| TForall v1 k1 t1	<- t
	, TForall v2 k2 t2	<- s
	, v1 == v2
	, k1 == k2
	= subsumes1 table t1 t2

	-- sums
	| TSum tKind ts		<- t
	, TSum sKind ss		<- s
	, tKind == sKind
	= or $ map (\si -> subsumes1 table t si) ss
	
	-- sum / single
	| TSum k ts		<- t
	, elem k [KEffect, KClosure]
	= or $ map (\ti -> subsumes1 table ti s) ts

	-- single / sum
	| TSum k ss		<- s
	, elem k [KEffect, KClosure]
	= and $ map (\si -> subsumes1 table t si) ss

	-- masks
	| TMask k t1 t2		<- t
	, TMask k s1 s2		<- s
	, subsumes1 table t1 s1
	, t2 == s2
	= True 


	-- SubFun
	-- fun
 	| TFunEC t1 t2 tEff tClo	<- t
	, TFunEC s1 s2 sEff sClo	<- s
	, subsumes1 table s1 t1
	, subsumes1 table t2 s2
	, subsumes1 table tEff sEff
	, subsumes1 table tClo sClo
	= True


	-- SubReplay
	-- hmm, perhaps should be using separate constraints, 
	--	and not substituting effects and closures into types.
	--
	-- G[e :> E] |- (a -(e)> b)  <: (a -(E)> b)
	--
	| TFunEC t1 t2 tEff@(TVar KEffect vE) tClo	<- s
	, Just tE		<- Map.lookup vE table
	, subsumes1 table t (TFunEC t1 t2 tE tClo)
--	= warning stage
--		("subsumes: Used SubReplay for (" % s % ")\n")
	=	True
	
	-- SubCtor
	--	BUGS: doesn't handle contra variant vars
	-- data 
	| TData tVar ts		<- t
	, TData sVar ss		<- s
	, tVar == sVar
	, length ts == length ss
	, and $ zipWith (subsumes1 table) ts ss
	= True
	

	-- This is really Eq
	--	T <: T
	
	-- closure constructor
	-- 	It doesn't matter what the variable is
	--	So long as the type is the same

	| TFree tVar ts		<- t
	, TFree sVar ss		<- s
	, ts == ss
	= True
	
	--
	| otherwise
	= False

