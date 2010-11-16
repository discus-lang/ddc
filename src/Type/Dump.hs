{-# OPTIONS -fno-warn-incomplete-record-updates #-}

module Type.Dump
	( dumpGraph
	, dumpInst
	, dumpSub)
where
import DDC.Main.Pretty
import DDC.Solve.State.Base
import DDC.Solve.State.Squid
import DDC.Solve.State.Graph
import DDC.Type.Kind
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
 ppr cls
  = case cls of
	ClassUnallocated
	 -> ppr "ClassUnallocated"

	ClassForward cid cid'
	 -> cid % " ==> " % cid' % "\n"
	
	ClassFetter { classFetter = f }
	 	-> "+" % classId cls % "\n"
		%  "        " % f % "\n"
		%  "\n\n"

	ClassFetterDeleted cls'
	 -> "DELETED " % cls'

 	Class{}
	 -> vcat
	 	-- class id / name / kind 
	   	$ (classId cls % " :: " % classKind cls)

		-- unified type
		: (case classUnified cls of
			Nothing	-> []
			Just t	-> ["     == " % ppr t, blank])

		-- aliases
		++ ["     ~  " %> (padL 30 t %% i) | (t, i) <- Map.toList $ classAliases cls]

		-- node types contributing to this class
		++ (if isInjectiveKind (classKind cls)
			then 		["     :> " %> (padL 30 t %% i) | (t, i) <- classTypeSources cls]
			else 		["     =  " %> (padL 30 t %% i) | (t, i) <- classTypeSources cls])

		-- class fetters
		++ (case Map.toList $ classFetters cls of
			[]	-> []
			_	->	["     +  " %> (padL 30 t %% i) | (t, i) <- Map.toList $ classFetters cls])
	
		-- multi-parameter type classes
		++ (if Set.null $ classFettersMulti cls
			then	[]
			else	map ppr $ Set.toList $ classFettersMulti cls)
			
		++ [blank]


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
