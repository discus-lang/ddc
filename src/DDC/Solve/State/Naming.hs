{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Functions dealing with the names of equivalence classes in the graph.
module DDC.Solve.State.Naming
	( instVar
	, newVarN
	, lookupSigmaVar
	, lookupVarToClassId
	, getCanonicalNameOfClass
	, addAliasForClass )
where
import DDC.Solve.State.Class
import DDC.Solve.State.Graph
import DDC.Solve.State.Base
import DDC.Solve.State.Squid
import DDC.Solve.State.Sink
import DDC.Type
import DDC.Var
import DDC.Main.Error
import DDC.Main.Pretty
import Type.Location
import Data.List
import Control.Monad
import qualified Data.Map	as Map

stage	= "DDC.Solve.Naming"

-- | Instantiate a variable.
instVar :: Var -> SquidM (Maybe Var)
instVar var
 = do	let space	= varNameSpace var

	-- lookup the generator for this namespace
	varGen		<- getsRef stateVarGen
	let mVarId	= Map.lookup space varGen
	instVar' var space mVarId

instVar' var space mVarId
 = case mVarId of
	Nothing
	 -> freakout stage
	  	("instVar: can't instantiate var in space " % show space
	  	% " var = " % show var)
		$ return Nothing
		
	Just vid
	 -> do
		-- increment the generator and write it back into the table.
		let vid'	= incVarId vid

		stateVarGen `modifyRef`
			\varGen -> Map.insert space vid' varGen

		-- the new variable remembers what it's an instance of..
		let name	= pprStrPlain vid
		let var'	= (varWithName name)
			 	{ varNameSpace	= varNameSpace var
			 	, varId		= vid }

		return $ Just var'

	

-- | Make a new variable in this namespace
newVarN :: NameSpace ->	SquidM Var
newVarN	space	
 = do 	Just vid	<- liftM (Map.lookup space)
			$  getsRef stateVarGen
	
	let vid'	= incVarId vid

	stateVarGen `modifyRef` \varGen -> 
		Map.insert space vid' varGen
	
	let name	= pprStrPlain vid
	let var'	= (varWithName name)
			{ varNameSpace	= space 
			, varId		= vid }
			
	return var'


-- | Lookup the type variable corresponding to this value variable.
lookupSigmaVar :: Var -> SquidM (Maybe Var)
lookupSigmaVar	v
 	= liftM (Map.lookup v)
	$ getsRef stateSigmaTable


-- | Lookup the class with this name.
--   Classes can have many different names, so this is a many-to-one map.
lookupVarToClassId :: Var -> SquidM (Maybe ClassId)
lookupVarToClassId v
 = do	graph		<- getsRef stateGraph
 	let vMap	= graphVarToClassId graph

	case Map.lookup v vMap of
	 Nothing	-> return Nothing
	 Just cid	
	  -> do	cid'	<- sinkClassId cid
	  	return	$ Just cid'


-- | Get the canonical name for a class.
--	If the class already has names in the alias list, then choose the shortest one.
--	Otherwise, make a fresh alias and use that.
getCanonicalNameOfClass :: ClassId -> SquidM Var
getCanonicalNameOfClass !cid
 = do 	Just cls	<- lookupClass cid

	case cls of
	 ClassForward _ cid'
	    -> getCanonicalNameOfClass cid'
		
	 -- The class already has a canonical name we can use.	
	 Class { className = Just name }	
	   -> return name
			
	 -- The class has no existing aliases, we we have to invent a name for it.
	 Class	{ className	= Nothing
		, classKind	= kind
		, classAliases	= aliases }
		
	  | Map.null aliases
	  , Just nameSpace	<- spaceOfKind $ resultKind kind
	  -> do	var		<- newVarN nameSpace
		let tSource	= TSI $ SIClassName
		addAliasForClass cid tSource var kind
		return var
	
	  | Map.null aliases
	  -> panic stage $ "no name space for kind " % kind % "\n"
	
	  -- The class existing aliases, so we can choose one to be the canonical name.
   	  | otherwise
	  -> let classNameOrd v1 v2
			| length (varName v1) < length (varName v2)	= Prelude.LT
			| length (varName v1) > length (varName v2)	= Prelude.GT
			| otherwise					= Prelude.EQ

	     in	return 	$ head 
			$ sortBy classNameOrd 
			$ Map.keys $ classAliases cls

	 _ -> panic stage 
		$ "getCanonicalNameOfClass: class " % cid % "has no name."


-- | Add an alias for a class.
--   An alias is a name that identifies the class. There can be many aliases 
--   for a given class, but only one ''canonical'' name.
addAliasForClass 
	:: ClassId		-- ^ cid of the class we're adding to.
	-> TypeSource		-- ^ Source of the name.
	-> Var			-- ^ The new name to add.
	-> Kind			-- ^ Kind of the name.
	-> SquidM ()

addAliasForClass cid src var kind
 = do	modifyClass cid
 	 $ \cls -> case cls of
		ClassUnallocated{}
		 -> (classEmpty cid kind src) 
			{ classAliases = Map.singleton var src }
			
		Class{}
		 -> cls	{ classAliases = Map.insert var src (classAliases cls) }

		_ -> panic stage 
			$ "addAliasForClass: can't modify class " % cid
			
	stateGraph `modifyRef` \graph -> 
		graph { graphVarToClassId = Map.insert var cid (graphVarToClassId graph) }

