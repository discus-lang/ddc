
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
 	Class{}
	 -> pretty
	 	$ classKind c 
		% " Class " 		% (padR 5 $ pretty $ classId c) % "\n"
		% "  backRef = "	% (Set.toList $ classBackRef c) 	% "\n"
		% "  type    = " 	% classType c				% "\n"
		% "\n"
		% (concat
			$ map pretty
			$ map (\(t, i) -> "    " % padR 70 (pretty t) % " " % i % "\n")
			$ (classNodes c))
		% "\n\n"
	

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

updateC c@Class{}
 = do	cid'		<- updateVC $ classId	   c
 	backRef'	<- updateVC $ classBackRef c
	type'		<- updateVC $ classType    c
	
	return	$ c
		{ classId	= cid'
		, classBackRef	= backRef'
		, classType	= type' }

		
		
 
 
