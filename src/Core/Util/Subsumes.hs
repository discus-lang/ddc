
module Core.Util.Subsumes
	(subsumes)

where

import Core.Exp
import Core.Pretty
import Core.ReconKind
import Core.Util.Bits
import Core.Util.Trim
import Shared.Pretty
import Shared.Error
import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Debug.Trace

-----
debug	= False
trace ss x	
 	= if debug 
		then Debug.Trace.trace (pprStrPlain ss) x
		else x

-- stage	= "Core.Util.Subsumes"
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
   in	{-# SCC "subsumes" #-} subsumes1 tableMore (stripTFree t) (stripTFree s)
	
subsumes1 table t s
 = let (ans, rule)	= subsumes2 table t s
   in  trace 	("subsumes (T :> S) |- " % ans	% "\n"
   		% " rule = " %> rule		% "\n"
	 	% "    T = " %> t 		% "\n"
		% "    S = " %> s		% "\n"
		% "   with " %> "\n" %!% (map (\(v, t) -> v % " :> " % t) $ Map.toList table) % "\n")
		ans

-- make sure closure terms are trimmed before comparing them
subsumes2 table t s
 = let trimC tt
 	| kindOfType tt == KClosure	= trimClosureC Set.empty Set.empty tt
	| otherwise			= tt
	
   in	subsumes3 table (trimC t) (trimC s)

subsumes3 table t s

	-- load up embedded TVarMore constraints
	| TVarMore tKind tVar tMore <- t
	= let table'	= slurpMore (tVar, tMore) table
	  in  (subsumes1 table' (TVar tKind tVar) s, "load VarMore")

	| TVarMore sKind sVar sMore <- s
	= let table'	= slurpMore (sVar, sMore) table
	  in  (subsumes1 table' t (TVar sKind sVar), "load VarMore")


	-- load up embedded FMore constraints
	| TFetters t' fs	<- t
	= let table'	= foldl (\tab (FMore v1 t2) -> slurpMore (v1, t2) tab) table fs
	  in  subsumes3 table' t' s
	

	| TFetters s' fs	<- s
	= let table'	= foldl (\tab (FMore v1 t2) -> slurpMore (v1, t2) tab) table fs
	  in  subsumes3 table' t s'


	-- SubRefl
	| t == s
	= (True, "SubRefl")

	-- SubTop
	-- top subsumes everything
	| TTop _		<- t
	= (True, "SubTop")

	-- SubBot
	-- anything subsumes bottom
	| TBot _		<- s
	= (True, "SubBot")

	-- SubTrans
	| TVar tKind tVar 	<- t
	, Just s2		<- Map.lookup tVar table 
	, subsumes table s2 s
	= (True, "SubTrans")

	-- SubVar
	-- G[t :> S] |- S <: t
	| TVar tKind tVar	<- t
	, Just s2		<- Map.lookup tVar table
	, s == s2
	= (True, "SubVar")
	

	-- SubAll
	| TForall v1 k1 t1	<- t
	, TForall v2 k2 t2	<- s
	, v1 == v2
	, k1 == k2
	= (subsumes1 table t1 t2, "SubAll")

	-- sums
	| TSum tKind ts		<- t
	, TSum sKind ss		<- s
	, tKind == sKind
	, and $ map (\si -> subsumes1 table t si) ss
	= (True, "SubSum - sums")
	
	-- sum / single
	| TSum k ts		<- t
	, elem k [KEffect, KClosure]
	, or $ map (\ti -> subsumes1 table ti s) ts
	= (True, "SubSum - sum single")

	-- single / sum
	| TSum k ss		<- s
	, elem k [KEffect, KClosure]
	, and $ map (\si -> subsumes1 table t si) ss
	= (True, "SubSum - single sum")

	-- masks
	| TMask k t1 t2		<- t
	, TMask k s1 s2		<- s
	, subsumes1 table t1 s1
	, t2 == s2
	= (True, "SubMask")

	| TMask k t1 t2		<- t
	, subsumes1 table t1 s
	= (True, "SubMask2")

	-- SubFun
	-- fun
 	| TFunEC t1 t2 tEff tClo	<- t
	, TFunEC s1 s2 sEff sClo	<- s
	, subsumes1 table s1 t1
	, subsumes1 table t2 s2
	, subsumes1 table tEff sEff
	, subsumes1 table tClo sClo
	= (True, "SubFun")

	-- SubTag
	| TFree _ t1			<- t
	, s1				<- s
	, subsumes1 table t1 s1
	= (True, "SubTag - tag t")
	
	| t1				<- t
	, TFree _ s1			<- s
	, subsumes1 table t1 s1
	= (True, "SubTag - tag s")

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
	= (True, "SubReplay")
	
	-- SubCtor
	--	TODO: If we knew which of these args was covar/contravar we
	--	could do a proper subsumption, but we don't have that info here. 
	--	Just take all the args as being invariant instead.
	--
	| TData tVar ts		<- t
	, TData sVar ss		<- s
	, tVar == sVar
	, length ts == length ss
--	, and $ zipWith (subsumes1 table) ts ss
	, ts == ss
	= (True, "SubCtor")
	

	-- This is really Eq
	--	T <: T
	
	-- closure constructor
	-- 	It doesn't matter what the variable is
	--	So long as the type is the same

	| TFree tVar ts		<- t
	, TFree sVar ss		<- s
	, ts == ss
	= (True, "SubFree - both")
	
	| TFree tVar ts		<- t
	, ts == s
	= (True, "SubFree - t")

	| TFree tVar ss		<- s
	, t == ss
	= (True, "SubFree - s")

	
	--
	| otherwise
	= (False, "fail")


-- Add the fact that T[v1] :> t2 to the table
--	In the case that t2 has more constraints, ie
--	v1 :> v2 :> t3, we get a chain of constraints added to the table.
-- ie	
--	v1 :> v2
--	v2 :> t3
--
slurpMore (v1, t2) table
 = case t2 of
--	TSum k ts
--	 -> foldl (\tab t -> slurpMore (v1, t) tab) table ts

 	TVarMore k v2 t3
	 -> let	table'	= Map.insert v1 (TVar k v2) table
	    in	slurpMore (v2, t3) table'
	    
	_ -> Map.insert v1 t2 table



stripTFree tt
 = case tt of
 	TSum k ts			-> makeTSum k $ map stripTFree ts
	TFree v t@(TVar KClosure _)	-> t
	_				-> tt
