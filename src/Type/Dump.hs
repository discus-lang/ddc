
module Type.Dump
	( dumpGraph
	, dumpGens
	, dumpSchemes
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

dumpGraph ::	SquidM String
dumpGraph
 = do
	school		<- gets stateGraph
	classes		<- liftIO (getElems $ graphClass school)
	classesU	<- mapM updateC classes
	
	return	$ pretty
		$ "===== Equiv Clases =====================================\n"
		% concat (zipWith prettyClass [0..] classesU)
		% "\n\n"

dumpSchemes :: 	SquidM String
dumpSchemes	= return "no schemes"


dumpGens :: 	SquidM String
dumpGens
 = do
 	-- Generalisations
{-	gens		<- gets stateGen
	
	return	$ pretty
		$ "===== Generalisations =========================================\n"
		% (concat $ map pretty $ map prettyVTE $ Map.toList gens)
		% "\n\n"
-}
	return ""
		
prettyVTE (v, (t, env))
 =   padR 20 (pretty v) % "\n" 
 %> ( ":: " % (pretty $ prettyTS t) % "\n"
    % ":- ENV = " % env) % "\n\n"
		
	
dumpInst :: 	SquidM String
dumpInst
 = do
 	-- Instantiations
	mInst		<- gets stateInst

	return	$ pretty
		$ "===== Instantiations ========================================\n"
		% prettyVMap mInst
		% "\n\n"

dumpSub :: SquidM String
dumpSub
 = do
	-- VarSub
	mVarSub		<- gets stateVarSub
		
	-----
	return 	$ pretty 
		$ "===== Var Sub ===============================================\n"
		% prettyVMap mVarSub
		% "\n\n"




-----	
prettyClass (ix :: Int) c
 = case c of
	ClassNil{}	-> []

	ClassForward c	-> []		
{-	ClassForward c
	 -> pretty
	 	$ ". ClassForward @" % ix % " ==> " % c % "\n\n"
-}
	ClassFetter { classFetter = f }
	 -> pretty
	 	$ "Class +" % classId c % "\n"
		% "        " % f % "\n"
		% "\n\n"

 	Class{}
	 -> pretty
		$ "Class " % classKind c % classId c 
		%> ("\n" % ":: " % liftM prettyTS (classType c))	% "\n\n"
		% "        -- queue\n"
		% "        " % classQueue c				% "\n"
		% "        -- back refs\n"
		% "        " % (Set.toList $ classBackRef c) 		% "\n"
		% "\n"
		% "        -- nodes\n"
		% (concat
			$ map pretty
			$ map (\(t, i) -> "        " %> (i % "\n" % prettyTS t) % "\n\n")
			$ (classNodes c))
		% "\n"
	

prettyVMap 	m
	= concat
	$ map (\(v, t) -> (padR 20 $ pretty v) ++ " = " ++ (pretty t) ++ "\n")
	$ Map.toList m
	
prettyVMapT 	m
	= concat
	$ map pretty
	$ map (\(v, t) 
		-> (padR 20 $ pretty v) 	% "\n"
		%> (":: " % (pretty $ prettyTS t)) % "\n\n")
	$ Map.toList m
 	 
 
-----
updateC :: Class -> SquidM Class
updateC c@ClassForward{}	= return c
updateC c@ClassNil{}		= return c
updateC c@ClassFetter { classFetter = f }
 = do	cid'		<- updateVC $ classId 	c
 	fetter'		<- updateVC f
	return	$ c 
		{ classId	= cid'
		, classFetter	= fetter' }

updateC c@Class{}
 = do	cid'		<- updateVC $ classId	   c
 	backRef'	<- updateVC $ classBackRef c
	
	typ'		<- case classType c of
				Nothing	-> return Nothing
				Just t	-> do
					typ2	<- updateVC t
					return	$ Just typ2
	
	return	$ c
		{ classId	= cid'
		, classBackRef	= backRef'
		, classType	= typ' }

		
		
 
 
