
module Type.Link
	( linkType
	, linkFetter )
where
import Type.Location
import DDC.Solve.State
import DDC.Main.Error
import DDC.Type
import DDC.Var
import Util
import qualified Data.Map	as Map

stage	= "Type.Link"

-- | Link the free vars in this type into the graph, but don't break it up into nodes. 
--
--   We link schemes from closures into the graph instead of feeding them
--   because they are 'finished' and no longer require unification. 
--
linkType 
 	:: [Var]		-- ^ Vars bound by a forall in this context. Don't link these vars.
	-> TypeSource		-- ^ Source of type constraint.
	-> Type 		-- ^ Type to link.
	-> SquidM Type		-- ^ Linked type.
	
linkType bound src tt
 = case tt of
	TForall b k t
	 -> do	let Just v	= takeVarOfBind b
		t'	<- linkType (v : bound) src t
	 	return	$ TForall b k t'

	TConstrain t crs
	 -> do	t'	<- linkType bound src t
		crs'	<- linkConstraints bound src crs
		return	$  TConstrain t' crs'

	TSum k es
	 -> do	es'	<- mapM (linkType bound src) es
	 	return	$ TSum k es'

	TApp t1 t2
	 -> do	t1'	<- linkType bound src t1
	 	t2'	<- linkType bound src t2
		return	$ TApp t1' t2'
		
	TCon{} 
	 -> return tt

 	TVar k (UVar v)
	 | elem v bound	-> return tt
	 | otherwise
	 -> do	mCid	<- lookupVarToClassId v
	 	case mCid of
		 Nothing 
		  -> do	cid	<- makeClassFromVar src k v
			return	$ TVar k $ UClass cid
			
		 Just cid
		  -> do	return	$ TVar k $ UClass cid
		
	-- don't link error types to the graph.
	TError{}	-> return tt

	-- cids are already linked.
	TVar k (UClass cid)
		-> return tt

	_ 	-> panic stage
		$ "linkType: cannot link " % tt


-- | Link the free vars in this fetter into the graph, but don't break it up into nodes. 
linkFetter
	:: [Var]		-- ^ Vars bound by a forall in this context. Don't link these vars.
	-> TypeSource		-- ^ Source of the fetter.
	-> Fetter 		-- ^ Fetter to link.
	-> SquidM Fetter	-- ^ Linked fetter.
	
linkFetter bound src ff
 = case ff of
 	FConstraint v ts
	 -> do	ts'	<- mapM (linkType bound src) ts
	 	return	$ FConstraint v ts'
		
	FProj pj v tDict tBind
	 -> do	tDict'	<- linkType bound src tDict
		tBind'	<- linkType bound src tBind
		return	$ FProj pj v tDict' tBind'


-- | Link the free vars in these constraints into the graph.
linkConstraints 
	:: [Var]
	-> TypeSource
	-> Constraints
	-> SquidM Constraints
	
linkConstraints bound src crs
 = do	let linkTT (t1, t2)
	     = do t1'	<- linkType bound src t1
		  t2'	<- linkType bound src t1
		  return (t1, t2')
		
	crsEq'		<- liftM Map.fromList $ mapM linkTT $ Map.toList $ crsEq   crs
	crsMore'	<- liftM Map.fromList $ mapM linkTT $ Map.toList $ crsMore crs
	crsOther'	<- mapM (linkFetter bound src) $ crsOther crs
	
	return	$ Constraints crsEq' crsMore' crsOther'





