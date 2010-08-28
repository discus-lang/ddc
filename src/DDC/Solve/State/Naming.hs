{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Functions dealing with the names of equivalence classes in the graph.
module DDC.Solve.State.Naming
	( instVar
	, getCanonicalNameOfClass)
where
import DDC.Solve.State.Base
import DDC.Solve.State.Squid
import DDC.Solve.State.Graph
import DDC.Solve.Location
import DDC.Type
import DDC.Var
import DDC.Main.Error
import DDC.Main.Pretty
import Data.List
import qualified Data.Map	as Map

stage	= "DDC.Solve.State.Naming"


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
		addAliasForClass cid kind tSource var
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



