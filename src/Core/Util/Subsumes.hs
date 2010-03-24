
-- | Checking of type subsumption.
--	The subsumption part is only applied to the effect and closure
--	portion of a type. We don't do subsumption on the value or region part.
--
module Core.Util.Subsumes
	(subsumes)
where
import Core.Util.Trim
import Type.Util.Bits
import Type.Util.Kind
import Type.Exp
import Util
import DDC.Main.Pretty
import DDC.Var
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import qualified Debug.Trace

-----
debug	= False
trace ss x	
 	= if debug 
		then Debug.Trace.trace (pprStrPlain ss) x
		else x


-- | Check if t subsumes s another. t :> s
--   Note that subsumption only works on the effect and closure information.
--   We don't need to worry about the difference between co and contra variance
--   because :> constraints are not restrictions on what value we can
--   use as a function parameter
--
subsumes 
	:: Map Var Type 	-- table of (v :> t) constraints
	-> Type 
	-> Type 
	-> Bool

subsumes tableMore t s
 = let 	?t		= t
	?s		= s
   in	{-# SCC "subsumes" #-} 
		subsumes1 tableMore (stripTFree t) (stripTFree s)
	
subsumes1 table t s
 = let (ans, rule)	= subsumes2 table t s
   in  trace 	("subsumes (T :> S) |- " % ans	% "\n"
   		% " rule = " %> rule		% "\n"
	 	% "    T = " %> t 		% "\n"
		% "    S = " %> s		% "\n"
		% "   with " %> "\n" %!% (map (\(v, t) -> v % " :> " % t) 
				$ Map.toList table) % "\n")
		ans

-- Make sure closure terms are trimmed before comparing them,
--	not much point checking a term that'll be trimmed out later anyway.
--	
subsumes2 table t s
 = let trimC tt
 	| isClosure tt
	= trimClosureC Set.empty Set.empty tt

	| otherwise			
	= tt
	
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
	= let table'	= foldl (\tab (FMore (TVar _ v1) t2) 
				-> slurpMore (v1, t2) tab) 
				table fs
	  in  subsumes3 table' t' s
	

	| TFetters s' fs	<- s
	= let table'	= foldl (\tab (FMore (TVar _ v1) t2) 
				-> slurpMore (v1, t2) tab) 
				table fs
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

	-- SubVarTrans
	--	This handles both SubVar and SubTrans.
	| TVar tKind tVar 	<- t
	, Just s2		<- Map.lookup tVar table 
	, subsumes table s2 s
	= (True, "SubTrans")

	-- sums
	| TSum tKind ts		<- t
	, TSum sKind ss		<- s
	, tKind == sKind
	, and $ map (\si -> subsumes1 table t si) ss
	= (True, "SubSum - sums")
	
	-- sum / single
	| TSum k ts		<- t
	, elem k [kEffect, kClosure]
	, or $ map (\ti -> subsumes1 table ti s) ts
	= (True, "SubSum - sum single")

	-- single / sum
	| TSum k ss		<- s
	, elem k [kEffect, kClosure]
	, and $ map (\si -> subsumes1 table t si) ss
	= (True, "SubSum - single sum")

	-- SubFun
	-- fun
 	| Just (t1, t2, tEff, tClo)	<- takeTFun t
	, Just (s1, s2, sEff, sClo)	<- takeTFun s
	, subsumes1 table t1 s1
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

	-- SubCtor
	| Just (tVar, k, ts)	<- takeTData t
	, Just (sVar, k, ss)	<- takeTData s
	, tVar == sVar
	, length ts == length ss
	, and $ zipWith (subsumes1 table) ts ss
	= let result
		| ts == ss 	
		= (True, "SubCtor")

		| and $ zipWith (subsumes1 table) ts ss
		= (True, "SubCtor")
	  in	result
			

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

	
	-- no dice.
	| otherwise
	= (False, "fail")


-- Add the fact that T[v1] :> t2 to the table.
--	In the case that t2 has more constraints, ie
--	v1 :> v2 :> t3, we get a chain of constraints added to the table.
-- ie	
--	v1 :> v2
--	v2 :> t3
--
slurpMore (v1, t2) table
 = case t2 of
 	TVarMore k v2 t3
	 -> let	table'	= Map.insert v1 (TVar k v2) table
	    in	slurpMore (v2, t3) table'
	    
	_ -> Map.insert v1 t2 table


stripTFree tt
 = case tt of
 	TSum k ts			-> makeTSum k $ map stripTFree ts
	TFree v t@(TVar k _)	
		| k == kClosure		-> t
	_				-> tt

