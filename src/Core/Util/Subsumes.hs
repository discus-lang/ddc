
module Core.Util.Subsumes
	(subsumes)

where

import Core.Exp
import Core.Pretty
import Shared.Error
import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Debug.Trace

debug	= False
trace ss x	
 	= if debug 
		then Debug.Trace.trace (pretty ss) x
		else x

--
stage	= "Core.Util.Subsumes"

-- | Check if t subsumes s another. t :> s
--	BUGS: assumes that effect and closures in data types are all covariant
--
subsumes :: Map Var Type -> Type -> Type -> Bool
subsumes tableMore t s
 = let 	?tableMore	= tableMore
	?t		= t
	?s		= s
   in	subsumes' t s
	
subsumes' t s
 = let ans	= subsumes3 t s
   in  trace ("subsumes " 
 		% ans 	% "\n"
	 	% t 	% "\n"
		% s 	% "\n") ans

subsumes3 t s

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
	, Just s2		<- Map.lookup tVar ?tableMore
	, s2 == s
	= True

	-- SubAll
	| TForall v1 k1 t1	<- t
	, TForall v2 k2 t2	<- s
	, v1 == v2
	, k1 == k2
	= subsumes' t1 t2

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


	-- SubFun
	-- fun
 	| TFunEC t1 t2 tEff tClo	<- t
	, TFunEC s1 s2 sEff sClo	<- s
	, subsumes' s1 t1
	, subsumes' t2 s2
	, subsumes' tEff sEff
	, subsumes' tClo sClo
	= True


	-- SubReplay
	-- hmm, perhaps should be using separate constraints, 
	--	and not substituting effects into types.
	--
	-- G[e :> E] |- (a -(e)> b)  <: (a -(E)> b)
	--
	| TFunEC t1 t2 tEff@(TVar KEffect vE) tClo	<- s
	, Just tE		<- Map.lookup vE ?tableMore
	, subsumes' t (TFunEC t1 t2 tE tClo)
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
	, and $ zipWith subsumes' ts ss
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

