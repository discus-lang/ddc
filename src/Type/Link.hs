
module Type.Link
	( linkType
	, linkFetter )
where
import Type.Util
import Type.Exp
import Type.Location
import Type.State
import Type.Class
import DDC.Main.Error
import DDC.Var
import Util

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
		
	TFetters t fs
	 -> do	t'	<- linkType bound src t
	 	fs'	<- mapM (linkFetter bound src) fs
		return	$ TFetters t' fs'

	TConstrain{}
	 -> linkType bound src $ toFetterFormT tt

	TSum k es
	 -> do	es'	<- mapM (linkType bound src) es
	 	return	$ TSum k es'

	TApp t1 t2
	 -> do	t1'	<- linkType bound src t1
	 	t2'	<- linkType bound src t2
		return	$ TApp t1' t2'
		
	TCon{} 
	 -> return tt

 	TVar k v
	 | elem v bound	-> return tt
	 | otherwise
	 -> do	mCid	<- lookupVarToClassId v
	 	case mCid of
		 Nothing 
		  -> do	cid	<- makeClassFromVar src k v
			return	$ TClass k cid
			
		 Just cid
		  -> do	return	$ TClass k cid
		
	-- don't link error types to the graph.
	TError{}	-> return tt

	-- cids are already linked.
	TClass k cid	-> return tt

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
		
	FWhere t1 t2
	 -> do	t1'	<- linkType bound src t1
	 	t2'	<- linkType bound src t2
	 	return	$ FWhere t1' t2'
			 
	FProj pj v tDict tBind
	 -> do	tDict'	<- linkType bound src tDict
		tBind'	<- linkType bound src tBind
		return	$ FProj pj v tDict' tBind'





