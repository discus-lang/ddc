
module Type.Dump
	( dumpGraph
	, dumpInst
	, dumpSub
	, prettyClass)
where

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Data.Array.IO	

import qualified Util.Data.Bag	as Bag
import Util.Data.Bag		(Bag)

import Util

import Type.Exp
import Type.Pretty
import Type.Util
import Type.State
import Type.Class

-- | dump the type graph
dumpGraph ::	SquidM String
dumpGraph
 = do
	school		<- gets stateGraph
	classes		<- liftIO (getElems $ graphClass school)
	classesU	<- mapM fowardCids classes
	
	return	$ pprStr
		$ "===== Equiv Clases =====================================\n"
		% concat (zipWith prettyClass [0..] classesU)
		% "\n\n"

		
-- | dump scheme instantiations
dumpInst :: 	SquidM String
dumpInst
 = do 	-- Instantiations
	mInst		<- gets stateInst
	return	$ pprStr
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
	return 	$ pprStr 
		$ "===== Var Sub ===============================================\n"
		% prettyVMap mVarSub
		% "\n\n"


-- | pretty print an equivalence class
prettyClass (ix :: Int) c
 = case c of
	ClassNil{}	-> []

	ClassForward c	-> []		
{-	ClassForward c
	 -> pprStr
	 	$ ". ClassForward @" % ix % " ==> " % c % "\n\n"
-}
	ClassFetter { classFetter = f }
	 -> pprStr
	 	$ "+" % classId c % "\n"
		% "        " % f % "\n"
		% "\n\n"

 	Class{}
	 -> pprStr
		-- class id / name / kind 
		$ classKind c 
			% classId c 
			% (case className c of 
				Nothing	-> pNil
				Just n	-> ppr n)			% "\n"
	
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
		% (concat
			$ map pprStr
			$ map (\(t, i) -> "        " %> (i % "\n" % prettyTS t) % "\n\n")
			$ (classNodes c))
		% "\n"
	

prettyVMap 	m
	= concat
	$ map (\(v, t) -> (padR 20 $ pprStr v) ++ " = " ++ (pprStr t) ++ "\n")
	$ Map.toList m
	
prettyVMapT 	m
	= concat
	$ map pprStr
	$ map (\(v, t) 
		-> (padR 20 $ pprStr v) 	% "\n"
		%> (":: " % (pprStr $ prettyTS t)) % "\n\n")
	$ Map.toList m
 	 
 
-- Rewrite this class so all its classIds are in canconical form.
fowardCids :: Class -> SquidM Class
fowardCids c@ClassForward{}	= return c
fowardCids c@ClassNil{}		= return c
fowardCids c@ClassFetter { classFetter = f }
 = do	cid'		<- updateVC $ classId 	c
 	fetter'		<- updateVC f
	return	$ c 
		{ classId	= cid'
		, classFetter	= fetter' }

fowardCids c@Class{}
 = do	cid'		<- updateVC $ classId	   c

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

		
		
 
 
