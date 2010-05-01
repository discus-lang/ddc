{-# OPTIONS -fno-warn-incomplete-record-updates #-}

module Type.Feed
	( feedConstraint
	, feedType 
	, feedFetter
	, addFetter
	, addFetterSource)
where
import Constraint.Exp
import Type.Exp
import Type.Builtin
import Type.Location
import Type.State
import Type.Class
import Type.Util
import Type.Link
import Util
import DDC.Main.Error
import DDC.Var
import qualified Data.Map	as Map
import qualified Data.Set	as Set

-----
stage	= "Type.Feed"

debug	= False
trace ss
 = when debug $ traceM ss



-- feedConstraint ----------------------------------------------------------------------------------
-- | Add a new constraint to the type graph.
--
feedConstraint 
	:: CTree -> SquidM ()
	
feedConstraint cc
 = trace ("feedConstraint: " % cc % "\n") >>
   case cc of
	-- Equality constraints. The LHS must be a variable.
 	CEq src (TVar k v1) t2
	 -> do
		-- create a new class for the LHS, with just that var in it.
	 	cid1		<- makeClassV src (kindOfSpace $ varNameSpace v1) v1

		-- feed the RHS into the graph.
		let ?src	= src
		Just (TClass _ cid2)
			<- feedType Nothing t2

		-- merge left and right hand sides.
		mergeClasses [cid1, cid2]
		return ()

	CEqs src ts
	 -> do	let ?src	= src
	 	Just ts'	<- liftM sequence
				$  mapM (feedType Nothing) ts
		let cids	= map (\(TClass k cid) -> cid) ts'
		mergeClasses cids
		return ()


	-- Signatures behave the same way as plain equality constraints.
	CSig src t1@(TVar k v1) t2
	 -> do 	feedConstraint (CEq src t1 t2)
		return ()

	-- Class constraints
	CClass src vC ts
	 -> do	let ?src	= src
		addFetter (FConstraint vC ts)
		return ()

	-- Projection constraints.
	CProject src j v1 tDict tBind
	 -> do	let ?src	= src
		addFetter (FProj j v1 tDict tBind)
		return ()		
				

	-- Type definitions, eg data constructors, external functions.
	CDef src (TVar k v1) t2
	 -> do	
	 	-- make a new class to hold the type.
		cid		<- makeClassV src (kindOfSpace $ varNameSpace v1) v1
	 
	 	-- add type to class
	 	addToClass cid src t2
		
		return ()


	_ -> 	panic stage
		 $ "feedConstraint: can't feed " % cc % "\n"

-- feedType --------------------------------------------------------------------------------------------
-- | Add a type to the type graph.
--	This always creates a new class and returns a classid.
--
feedType 	
	:: (?src :: TypeSource)
	=> Maybe ClassId
	-> Type -> SquidM (Maybe Type)

feedType mParent t
 = do	t'	<- feedType' mParent t
	return t'
	
feedType'	mParent t
 = case t of
	TFetters t fs
	 -> do	
	 	-- Rename the vars on the LHS of FLet bindings to make sure
	 	--	they don't conflict with any vars already in the graph.
		ttSub		<- liftM (Map.fromList . catMaybes)
				$  mapM (\f -> case f of
					FWhere t1@(TVar k v1) t2
					 -> do	let Just nameSpace	= spaceOfKind k
						v1'	<- newVarN nameSpace
					 	return	$ Just (t1, TVar k v1')

					_ -> 	return	$ Nothing)
				$ fs

		let fs2		= subTT_all ttSub fs
		let t2		= subTT_all ttSub t
				
		mapM_ (feedFetter mParent) fs2
	 	t3		<- feedType mParent t2
		return	t3

	TConstrain{}
	 -> feedType' mParent
	 $  toFetterFormT t

	TSum k ts
	 -> do 	cidE		<- allocClass (Just k)
		Just es'	<- liftM sequence
				$  mapM (feedType1 (Just cidE)) ts
		addNode cidE 	$ TSum k es'

		returnJ		$ TClass k cidE

	TApp t1 t2
	 -> do	
		let Just k	= kindOfType t
	 	cidT		<- allocClass (Just k)
	 	Just t1'	<- feedType (Just cidT) t1
		Just t2'	<- feedType (Just cidT) t2
		addNode cidT	$ TApp t1' t2'
		returnJ		$ TClass k cidT

	TCon tc
	 -> do	cidT		<- allocClass (Just $ tyConKind tc)
	 	addNode cidT	$ TCon tc
		returnJ		$ TClass (tyConKind tc) cidT

 	TVar k v 
	 -> do 	cidT		<- makeClassV ?src k v 
		returnJ		$ TClass k cidT

	TBot kind
	 -> do	cid		<- allocClass (Just kind)
		addNode cid	$ TBot kind
		returnJ		$ TClass kind cid

	TTop kind
	 -> do	cid		<- allocClass (Just kind)
		addNode cid	$ TTop kind
		returnJ		$ TClass kind cid

	-- effect
	TEffect v ts
	 -> do 	cidE		<- allocClass (Just kEffect)
		Just ts'	<- liftM sequence
				$  mapM (feedType (Just cidE)) ts
		addNode cidE 	$ TEffect v ts'
		returnJ		$ TClass kEffect cidE

	-- closure
	-- TFree's that we get from the constraints might refer to types that are in
	--	the defs table but not the graph. We don't want to pollute the graph
	--	with the whole external def so trim these types down and just add the
	--	closure information that we need.
	TFree v1 t@(TVar kV v2)
	 | kV == kValue
	 -> do	cid		<- allocClass (Just kClosure)
		defs		<- gets stateDefs
		case Map.lookup v2 defs of
		 -- type that we're refering to is in the defs table
		 Just tDef
		  -> do	let tDef_trim	= toFetterFormT 
					$ trimClosureT_constrainForm Set.empty Set.empty 
					$ toConstrainFormT
					$ flattenT tDef

		  	tDef'		<- linkType mParent [] tDef_trim

			-- we use addToClass here because we don't need to register effects and 
			--	classes for crushing in this type
			addToClass cid ?src $ TFree v1 tDef'
			returnJ		$ TClass kClosure cid

		 -- type must be in the graph
		 Nothing
		  -> do	t'		<- linkType mParent [] t
			addNode	cid	$ TFree v1 t'
			returnJ		$ TClass kClosure cid

	-- A non-var closure. We can get these from signatures and instantiated schemes
	TFree v1 t
	 -> do	cid		<- allocClass (Just kClosure)
		t'		<- linkType mParent [] t
		addNode	cid	$ TFree v1 t'
		returnJ		$ TClass kClosure cid

	TClass k cid
	 -> do 	cidT'		<- sinkClassId cid
		returnJ		$ TClass k cidT'
		
	_  ->	freakout stage
			( "feedType: cannot feed this type into the graph.\n"
			% "   type    = " % t 		% "\n"
			% "           = " % show t	% "\n"
			% "   source  = " % ?src 	% "\n"
			% "   mParent = " % mParent	% "\n")
			$ return Nothing

-----
-- feedType1
--	Feed a type into a type graph.
--	If the type is an Effect or Closure constructor then don't create a new class.
--	This is used to reduce the number of classes in the graph.
--
--	The constructors in sums from constraints such as: 
--		$e1 = ${!C a b, !D c d}
--	aren't points where unification can happen, so there is no reason to give them 
--	their own equivalence clases. 
--
--	This is unlike the effects from data or function constructors, ie.
--		t1  = a -(!e1 $c1)> b
--	here, t1 might be unified with another type, causing !e1 and $c1 to be unified 
--	also - so they need their own classes.
--

feedType1 
	:: (?src :: TypeSource)
	=> Maybe ClassId
	-> Type -> SquidM (Maybe Type)
		
feedType1 mParent tt
 = case tt of
	TTop k
	 | elem k [kEffect, kClosure]	
	 -> returnJ tt
	 
	TBot k
	 | elem k [kEffect, kClosure]
	 -> returnJ tt

	TSum k []
	 ->	returnJ tt
 
 	-- effects
 	TEffect v ts
	 -> do	Just ts' <- liftM sequence
	 		 $  mapM (feedType mParent) ts
	 	returnJ	$ TEffect v ts'
		
	-- closures
	TFree v t
	 -> do	Just tt'	<- feedType mParent tt
	 	returnJ	$ tt'

	_ 	-> feedType mParent tt


-- Fetter ------------------------------------------------------------------------------------------
feedFetter 
	:: (?src :: TypeSource)
	=> Maybe ClassId
	-> Fetter 
	-> SquidM ()

feedFetter	mParent f
 = trace ("feedFetter " % f % "\n") >>
   case f of
	FWhere t1 t2
	 -> do	Just (TClass k1 cid1)	<- feedType mParent t1
	 	Just (TClass k2 cid2)	<- feedType mParent t2
		mergeClasses [cid1, cid2]
		return ()

	FMore t1 t2	
	 -> 	feedFetter mParent (FWhere t1 t2)

	FConstraint v ts
	 -> do	addFetter f
	 	return ()

	FProj pj v tDict tBind
	 -> do	cidC		<- allocClass Nothing
	 	Just [tDict', tBind']	
				<- liftM sequence
				$  mapM (feedType (Just cidC)) [tDict, tBind]

		addFetter $ FProj pj v tDict' tBind'
		return ()
		


-----
addNode :: (?src :: TypeSource)
	=> ClassId
	-> Type
	-> SquidM ()
	
addNode    cidT	t
 = do	addToClass cidT	?src t
	activateClass cidT



-- addFetter ---------------------------------------------------------------------------------------
-- | Add a new fetter constraint to the graph
addFetterSource :: TypeSource -> Fetter -> SquidM Bool
addFetterSource src f 
	= let 	?src	= src
	  in	addFetter f


addFetter
	:: (?src :: TypeSource)
	=> Fetter
	-> SquidM Bool
	
-- Single parameter type class contraints are added directly to the equivalence
--	class which they constrain.
--
addFetter f@(FConstraint vC [t])
 = do	
	trace	$ "    * addFetter: " % f	% "\n\n"

	-- The target type t could be a TVar or a TClass
	--	Use feedType to make sure it's a TClass
	Just (TClass k cid)	
			<- feedType Nothing t
	
	-- add the fetter to the equivalence class
	let fNew	= FConstraint vC [TClass k cid]

	-- check what fetters are already there
	Just cls	<- lookupClass cid
	let vfsThere	= map (\(FConstraint v _, _) -> v) 
			$ classFetterSources cls

	-- if the fetter to be added is already there then there's no need to add it again.
	if (elem vC $ vfsThere)
	 then	return False
	 else do
 		modifyClass cid
	  	 $ \c -> c	
	 		{ classFetterSources	= (fNew, ?src) 	: classFetterSources c }

	 	activateClass cid
		return True
			
	
-- Multi parameter type class constraints are added as ClassFetter nodes in the graph 
--	and the equivalence classes which they constraint hold ClassIds which point to them.
--
addFetter f@(FConstraint v ts)
 = do 	
 	-- create a new class to hold this node
	cidF		<- allocClass Nothing
	 	
	-- add the type args to the graph
	Just ts'	<- liftM sequence
			$  mapM (feedType (Just cidF)) ts

	-- add the fetter to the graph
	let f	= FConstraint v ts'

	modifyClass cidF
	 $ \c -> ClassFetter
	 	{ classId	= cidF
		, classFetter	= f 
		, classSource	= ?src }

	activateClass cidF

	-- work out what cids this constraint is acting on
	let cids	= map (\(TClass k cid) -> cid) ts'
	
	-- add a reference to this constraint to all those classes
	mapM	(\cid -> modifyClass cid
			$ \c -> c { classFettersMulti = Set.insert cidF (classFettersMulti c) })
		$ cids

	return True

addFetter f@(FProj j v1 tDict tBind)
 = do
 	-- a new class to hold this node
 	cidF	<- allocClass Nothing
	
	-- add the type args to the graph
 	Just [tDict', tBind']
		<- liftM sequence
		$  mapM (feedType (Just cidF)) [tDict, tBind]

	-- add the fetter to the graph
	let f	= FProj j v1 tDict' tBind'
	
	modifyClass cidF
	 $ \c -> ClassFetter
	 	{ classId	= cidF
		, classFetter	= f 
		, classSource	= ?src }
		
	activateClass cidF
	
	-- add a reference to the constraint to all those classes it is acting on
	let cids	= map (\(TClass k cid) -> cid) [tDict', tBind']

	mapM	(\cid -> modifyClass cid
			$ \c -> c { classFettersMulti = Set.insert cidF (classFettersMulti c) })
		cids
			
	return True
	

