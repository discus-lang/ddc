
-- | Functions dealing with the names of
module DDC.Solve.Naming
	( lookupVarToClassId
	, getCanonicalNameOfClass
	, addAliasToClass )
where
import DDC.Solve.Sink
import DDC.Type
import DDC.Var
import DDC.Main.Error
import DDC.Main.Pretty
import Type.Location
import Type.State
import Type.Class
import Data.List
import Data.Array.IO
import Control.Monad.Trans
import qualified Data.Map	as Map

stage	= "DDC.Solve.Naming"

-- | Get the class of this name.
--   Classes can have many different names, so this is a many-to-one map.
--   If this function returns `Nothing` then there was no class with this name, 
--   meaning and something has gone badly wrong.
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
--	If the class already has names in the alias list then choose the one
--	when the smallest name, otherwise allocate a new variable and use that.
getCanonicalNameOfClass :: ClassId -> SquidM Var
getCanonicalNameOfClass cid_
 = do 	cid		<- sinkClassId cid_
	Just cls	<- lookupClass cid

	case cls of
	 -- The class already has a canonical name we can use.	
	 Class { className = Just name }	
	   -> return name
			
	 -- The class has no existing aliases, we we have to invent a name for it.
	 Class	{ className	= Nothing
		, classKind	= kind
		, classAliases	= [] }
		
	  | Just nameSpace	<- spaceOfKind $ resultKind kind
	  -> do	var		<- newVarN nameSpace
		let tSource	= TSI $ SIClassName
		addAliasToClass cid tSource var kind
		return var
		
	  | otherwise
	  -> panic stage $ "no name space for kind " % kind % "\n"
	
	 -- The class existing aliases, so we can choose one to be the canonical name.
	 Class 	{ className 	= Nothing
		, classKind 	= kind 
		, classAliases	= _:_ }

	  -> let classNameOrd v1 v2
			| length (varName v1) < length (varName v2)	= Prelude.LT
			| length (varName v1) > length (varName v2)	= Prelude.GT
			| otherwise					= Prelude.EQ
	     in	return 	$ head 
			$ sortBy classNameOrd 
			$ map fst $ classAliases cls


-- | Add a new alias to a class.
--   An alias is another name that identifies the class, so we don't have 

-- This is like addToClass, except that if we just give a class a new name
--	then we don't need to change its type
addAliasToClass 
	:: ClassId
	-> TypeSource
	-> Var
	-> Kind
	-> SquidM ()

addAliasToClass cid_ src v kind
 = do	let node	= NVar v
 	graph	<- getsRef stateGraph
 	cid	<- sinkClassId cid_
 	cls	<- liftIO (readArray (graphClass graph) cid)
	cls'	<- addNameToClass2 cid src node kind cls
	liftIO (writeArray (graphClass graph) cid cls')
	linkVar cid node
	return ()


addNameToClass2 cid src node kind cls
 = case cls of
 	ClassUnallocated
	 -> addNameToClass3 cid src node (classEmpty cid kind src)

	Class{}		
	 -> addNameToClass3 cid src node cls
	
addNameToClass3 cid src node cls
 = do 	activateClass cid
 	return	$ cls
		{ classTypeSources = (node, src) : classTypeSources cls }
