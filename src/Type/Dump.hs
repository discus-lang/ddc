{-# OPTIONS -fno-warn-incomplete-record-updates #-}

module Type.Dump
	( dumpGraph
	, dumpInst
	, dumpSub)
where
import DDC.Main.Pretty
import DDC.Solve.State
import Util
import Data.Array.IO	
import qualified Data.Map	as Map
import qualified Data.Set	as Set


-- | dump the type graph
dumpGraph ::	SquidM (PrettyM PMode)
dumpGraph
 = do
	school		<- getsRef stateGraph
	classes		<- liftIO (getElems $ graphClass school)
	classesU	<- mapM forwardCids classes
	
	return	$ "===== Equiv Clases =====================================\n"
		% dumpGraph' blank classesU
		% "\n\n"

dumpGraph' acc []	= acc
dumpGraph' acc (c:cs)
 = case c of
	ClassUnallocated	-> dumpGraph' acc cs
	_			-> dumpGraph' (acc % c) cs
		

-- | dump scheme instantiations
dumpInst :: 	SquidM String
dumpInst
 = do 	-- Instantiations
	mInst		<- getsRef stateInst
	return	$ pprStrPlain
		$ "===== Instantiations ========================================\n"
		% prettyVMap mInst
		% "\n\n"


-- | dump variable substitution
dumpSub :: SquidM String
dumpSub
 = do	mVarSub		<- getsRef stateVarSub
		
	-----
	return 	$ pprStrPlain
		$ "===== Var Sub ===============================================\n"
		% prettyVMap mVarSub
		% "\n\n"


-- | pretty print an equivalence class
instance Pretty Class PMode where
 ppr c 
  = case c of
	ClassUnallocated
	 -> ppr "ClassUnallocated"

	ClassForward cid cid'
	 -> cid % " ==> " % cid' % "\n"
	
	ClassFetter { classFetter = f }
	 	-> "+" % classId c % "\n"
		%  "        " % f % "\n"
		%  "\n\n"

	ClassFetterDeleted cls'
	 -> "DELETED " % cls'

 	Class{}
	 ->  	-- class id / name / kind 
	    	classId c
		% " :: " % classKind c % "\n"

		-- unified type
		%> (case classUnified c of
			Nothing	-> ppr "-- not unified --\n"
			Just t	-> ppr t % "\n")
		% "\n"

		-- aliases
		%> "-- aliases\n"
		% (punc "\n"
			$ map (\(v, loc) -> "        " %> (padL 20 v % loc)) 
				$ Map.toList $ classAliases c)
		% "\n\n"


		-- class fetters
		% (case Map.toList $ classFetters c of
			[]	-> blank
			_	-> "        -- fetters\n"
				%> "\n" %!% (Map.toList $ classFetters c) % "\n\n")
	
		-- multi-parameter type classes
		% (if Set.null $ classFettersMulti c
			then blank
			else 	   "        -- fetters multi-parameter\n"
				 % "        " % classFettersMulti c % "\n\n")
		
		-- node types contributing to this class
		% "        -- nodes\n"
		% (punc "\n\n"
			$ map (\(t, i) -> "        " %> (t % "\n" % i))
			$ classTypeSources c)
		% "\n\n"
	

prettyVMap 	m
	= punc "\n"
	$ map (\(v, t) -> padL 20 v % " = " % t)
	$ Map.toList m
	

-- TODO: Rewrite this class so all its classIds are in canconical form.
forwardCids :: Class -> SquidM Class
forwardCids cls
 = case cls of
	ClassUnallocated{}	-> return cls
	ClassForward{}		-> return cls
	ClassFetterDeleted{}	-> return cls

	ClassFetter { classFetter = fetter }
	 -> do	return	$ cls
			{ classFetter	= fetter }

	Class{}
	 -> do	return	cls
