
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

-----
stage	= "Type.Link"

-- Link the free vars in this type into the graph, 
--	but don't break it up into nodes. 
--
--	We link schemes from closures into the graph instead of feeding them
--	because they are 'finished' and no longer require unification. 
--
--	However, free variables may still need to be stubstituted.	
--	
--
linkType 
	:: (?src :: TypeSource)		-- source of type constraint
	=> Maybe ClassId		-- parent class
	-> [Var]			-- vars bound by a forall in this context. Don't link these vars.
	-> Type 
	-> SquidM Type
	
linkType mParent bound tt
 = case tt of
	TForall b k t
	 -> do	let Just v	= takeVarOfBind b
		t'	<- linkType mParent (v : bound) t
	 	return	$ TForall b k t'
		
	TFetters t fs
	 -> do	t'	<- linkType mParent bound t
	 	fs'	<- mapM (linkFetter mParent bound) fs
		return	$ TFetters t' fs'

	TConstrain{}
	 -> linkType mParent bound
	 $  toFetterFormT tt


	TSum k es
	 -> do	es'	<- mapM (linkType mParent bound) es
	 	return	$ TSum k es'

	TApp t1 t2
	 -> do	t1'	<- linkType mParent bound t1
	 	t2'	<- linkType mParent bound t2
		return	$ TApp t1' t2'
		
	TCon{} -> return	tt

 	TVar k v
	 | elem v bound	-> return tt
	 | otherwise
	 -> do	mCid	<- lookupVarToClassId v
	 	case mCid of
		 Nothing 
		  -> do	cid	<- makeClassV ?src k v
			return	$ TClass k cid
			
		 Just cid
		  -> do	return	$ TClass k cid

	-- effect
	TEffect v ts
	 -> do	ts'	<- mapM (linkType mParent bound) ts
	 	return	$ TEffect v ts'

	-- closure
	TFree v t
	 -> do	t'	<- linkType mParent bound t
	 	return	$ TFree v t'
		
	TDanger t1 t2
	 -> do	t1'	<- linkType mParent bound t1
	 	t2'	<- linkType mParent bound t2
		return	$ TDanger t1' t2'
		
	-- Don't link error types to the graph.
	--	It's probably not going to be useful.
	TError{}	-> return tt

	TClass k cid	-> return tt

	_ 	-> panic stage
		$ "linkType: cannot link " % tt


-----
linkFetter
	:: (?src :: TypeSource)		-- source of constraint
	=> Maybe ClassId		-- parent class
	-> [Var]			-- vars bound by a forall in this context. Don't link these vars.
	-> Fetter 
	-> SquidM Fetter
	
linkFetter mParent bound ff
 = case ff of
 	FConstraint v ts
	 -> do	ts'	<- mapM (linkType mParent bound) ts
	 	return	$ FConstraint v ts'
		
	FWhere t1 t2
	 -> do	t1'	<- linkType mParent bound t1
	 	t2'	<- linkType mParent bound t2
	 	return	$ FWhere t1' t2'
			 
	FProj pj v tDict tBind
	 -> do	tDict'	<- linkType mParent bound tDict
		tBind'	<- linkType mParent bound tBind
		return	$ FProj pj v tDict' tBind'





