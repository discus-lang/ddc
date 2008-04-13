{-# OPTIONS -fno-warn-incomplete-record-updates #-}

module Type.Dump
	( dumpGraph
	, dumpInst
	, dumpSub
	, prettyClass)
where


import Type.Exp
import Type.Pretty
import Type.Util
import Type.State
import Type.Class

import Shared.Pretty
import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Data.Array.IO	



-- | dump the type graph
dumpGraph ::	SquidM (PrettyM PMode)
dumpGraph
 = do
	school		<- gets stateGraph
	classes		<- liftIO (getElems $ graphClass school)
	classesU	<- mapM forwardCids classes
	
	return	$ "===== Equiv Clases =====================================\n"
		% dumpGraph' pNil classesU
		% "\n\n"

dumpGraph' acc []	= acc
dumpGraph' acc (c:cs)
 = case c of
 	ClassNil{}	-> dumpGraph' acc cs
	ClassForward{}	-> dumpGraph' acc cs
	_		-> dumpGraph' (acc % c) cs
	

		
-- | dump scheme instantiations
dumpInst :: 	SquidM String
dumpInst
 = do 	-- Instantiations
	mInst		<- gets stateInst
	return	$ pprStrPlain
		$ "===== Instantiations ========================================\n"
		% prettyVMap mInst
		% "\n\n"

-- | dump variable substitution
dumpSub :: SquidM String
dumpSub
 = do
	-- VarSub
	mVarSub		<- gets stateVarSub
		
	-----
	return 	$ pprStrPlain
		$ "===== Var Sub ===============================================\n"
		% prettyVMap mVarSub
		% "\n\n"

prettyClass :: Int -> Class -> PrettyM PMode
prettyClass ix c
 = case c of
 	ClassNil{}	-> pNil
	ClassForward{}	-> pNil
	_		-> ppr c


-- | pretty print an equivalence class
instance Pretty Class PMode where
 ppr c 
  = case c of
	ClassNil{}	-> ppr "ClassNil{}"

	ClassForward c
	 -> ". ClassForward ==> " % c % "\n\n"

	ClassFetter { classFetter = f }
	 	-> "+" % classId c % "\n"
		%  "        " % f % "\n"
		%  "\n\n"

 	Class{}
	 ->  	-- class id / name / kind 
		classKind c 
			% classId c 
			% (case className c of 
				Nothing	-> pNil
				Just n	-> ppr n) % "\n"
	
		-- class type
		%> (case classType c of
			Nothing	-> ppr "-- no type --\n"
			Just t	-> ":: " % prettyTS t % "\n\n")

		-- class fetters
		% (case classFetters c of
			[]	-> pNil
			_	-> "        -- fetters\n"
				%> "\n" %!% classFetters c % "\n\n")
	
		-- multi-parameter type classes
		% (if Set.null $ classFettersMulti c
			then pNil
			else 	   "        -- fetters multi-parameter\n"
				 % "        " % classFettersMulti c % "\n\n")
	
		-- unification queue
		% (case classQueue c of
			[]	-> pNil
			_	-> "        -- queue\n"
				%> "\n" %!% classQueue c % "\n\n")
		% "        -- nodes\n"
		% (punc "\n\n"
			$ map (\(t, i) -> "        " %> (i % "\n" % prettyTS t))
			$ (classNodes c))
		% "\n\n"
	

prettyVMap 	m
	= punc "\n"
	$ map (\(v, t) -> padL 20 v % " = " % t)
	$ Map.toList m
	

-- Rewrite this class so all its classIds are in canconical form.
forwardCids :: Class -> SquidM Class
forwardCids c@ClassForward{}	
	= return c

forwardCids c@ClassNil{}		
	= return c

forwardCids c@ClassFetter { classFetter = f }
 = do	cid'		<- updateVC $ classId  c
 	fetter'		<- updateVC f
	return	$ c 
		{ classId	= cid'
		, classFetter	= fetter' }

forwardCids c@Class{}
 = do	cid'		<- updateVC $ classId c

	fs'		<- mapM updateVC $ classFetters c

	let (ts, ns)	= unzip $ classNodes c
	ts'		<- mapM updateVC ts
	let nodes'	= zip ts' ns

	
	typ'		<- case classType c of
				Nothing	-> return Nothing
				Just t	-> do
					typ2	<- updateVC t
					return	$ Just typ2
	
	return	$ c
		{ classId	= cid'
		, classType	= typ' 
		, classFetters	= fs' 
		, classNodes	= nodes' }
