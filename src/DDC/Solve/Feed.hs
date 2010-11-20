{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# OPTIONS -fno-warn-incomplete-record-updates #-}

-- | ''Feeding'' refers to adding type constraints to the type graph.
module DDC.Solve.Feed
	( feedConstraint
	, feedType 
	, feedFetter
	, addFetter)
where
import Type.Link
import Type.Dump		()
import DDC.Solve.Interface.Problem
import DDC.Solve.Location
import DDC.Solve.State
import DDC.Constraint.Exp
import DDC.Main.Error
import DDC.Type
import Util
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import qualified Data.Sequence	as Seq

stage	 = "DDC.Type.Feed"
debug	 = False
trace ss = when debug $ traceM ss


-- Constraints ------------------------------------------------------------------------------------
-- | Add a single constraint to the type graph.
feedConstraint :: CTree -> SquidM ()
feedConstraint cc
 = trace ("feedConstraint\n" %> cc % "\n\n") >>
   case cc of
	-- Equality constraints. The LHS must be a variable.
 	CEq src (TVar k (UVar v1)) t2
	 -> do
		-- feed the RHS into the graph.
		cid2	<- feedType src t2

		-- We've got the class of the root node of the type, now we need to attach it
		-- to the name of the variable on the left of the constraint.
		--
		-- If there is no existing class containing this var then we can add it directly
		-- the one we just made, otherwise have have to merge the new and existing classes.
		--
		-- The first option is preferred, because we don't have to create a cid-cid
		-- substitution (ie add a forward to the graph) for the merge.
		mCid1	<- lookupCidOfVar v1
		case mCid1 of
		 Nothing	
		  -> 	addAliasForClass cid2 k src v1

		 Just cid1
		  -> do	mergeClasses [cid1, cid2]
			return ()


	-- The slurper sometimes gives us :> Bot constraints,
	--	ditch these right up front.
	CMore _ _ (TSum _ [])
	 -> 	return ()

	-- TODO: Handle this differently
	-- TODO  Check we don't have eq constraints for effects or closures.
	CMore src t1 t2
	 -> do	feedConstraint (CEq src t1 t2)
		return ()

	-- Multiple equality.
	-- Feed them all into the graph and merge the root classes.
	CEqs src ts
	 -> do	cids		<- mapM (feedType src) ts
		mergeClasses cids
		return ()

	-- Signatures behave the same way as plain equality constraints.
	CSig src t1@TVar{} t2
	 -> 	feedConstraint (CEq src t1 t2)

	-- Class constraints
	CClass src vC ts
	 -> do	addFetter src (FConstraint vC ts)
		return ()

	-- Projection constraints.
	CProject src j v1 tDict tBind
	 -> do	addFetter src (FProj j v1 tDict tBind)
		return ()		

	-- Other ``constraints'' carry class definitions etc, and can't be added directly
	-- to the graph. TODO: refactor constraint type to store these separately.
	_ -> 	panic stage
		 $ "feedConstraint: can't feed " % cc % "\n"


-- Types ------------------------------------------------------------------------------------------
-- | Add a type to the graph, returning the cid of the root node.
--   The type is converted to the node form on the way in.
feedType :: TypeSource -> Type -> SquidM ClassId
feedType src tt
 = case tt of
	-- We need to rename the vars on the LHS of FWhere bindings to make sure
	-- they don't conflict with vars arleady in the graph.
	TConstrain t crs
	 -> do 	let fs		= fettersOfConstraints crs

		-- Rename the vars on the LHS of bindints.
		ttSub		<- liftM (Map.fromList . catMaybes)
				$  mapM (\f -> case f of
					FWhere t1@(TVar k UVar{}) _
					 -> do	let Just nameSpace	= spaceOfKind k
						v1'	<- newVarN nameSpace
					 	return	$ Just (t1, TVar k (UVar v1'))

					_ -> 	return	$ Nothing)
				$ fs

		-- Substitute the reamings into the fetters and body type.
		let fs2		= subTT_everywhere ttSub fs
		let t2		= subTT_everywhere ttSub t

		-- Add the constraints to the graph.
		mapM_ (feedFetter src) fs2
		
		-- Add the body type.
	 	feedType src t2


	-- Empty classes have no nodes, but create the class anyway so we have its cid
	TSum k []
	 -> 	allocClass k src

	-- Add summations. 
	-- Each of the components is stored as its own node in the class.
	TSum k ts
	 -> do 	cidT	<- allocClass k src
		cids	<- mapM (feedType src) ts
		mapM_ (addNode cidT src k . NCid) cids
		return cidT

	-- TFree's that we get from the constraint slurper may refer to types that are
	-- in the defs table, and not in the graph.
	-- We don't want to pollute the graph with the whole external def, so trim these
	-- types down and just add the closure information that we need.
	-- 
	-- TODO: We're using this form to mean ''the type of some binding''.
	--       The variable refers to the binding, and is not a type variable in the true sense.
	--       We should use a different closure constructor to represent this so that it 
	--       doesn't clash with the next form that carries ''real'' types.
	--
	TApp{}
	 | Just (v1, t@(TVar kV (UVar v2)))	<- takeTFree tt
	 , isValueKind kV
	 -> do	cid		<- allocClass kClosure src
		env		<- gets stateEnv
		case Map.lookup v2 (squidEnvDefs env) of
		 -- type that we're refering to is in the defs table
		 Just (ProbDef _ _ tDef)
		  -> do	let tDef_trim	= trimClosureT $ flattenT tDef
		  	tDef'		<- linkType [] src tDef_trim

			-- we use addToClass here because we don't need to register effects and 
			-- classes for crushing in this type
			addNodeToClass cid kClosure src
				$ NFree v1 tDef'

			return cid

		 -- type must be in the graph
		 Nothing
		  -> do	t'		<- linkType [] src t
			addNode	cid src	kClosure
				$ NFree v1 t'

	-- A closure term containing a type expression.
	-- We allow full expressions like ''Int %r1'' in the constraints, but only
	-- need the trimmed form for inference.
	TApp{}
	 | Just (v1, t)	<- takeTFree tt
	 -> do	cid	<- allocClass kClosure src
		t'	<- linkType [] src $ trimClosureT t
		addNode	cid src kClosure	
			$ NFree v1 t'

	-- Type applications.
	TApp t1 t2
	 -> do 	cid1	<- feedType src t1
		cid2	<- feedType src t2
		newNode src (kindOfType tt) 
			$ NApp cid1 cid2
		
	-- Plain constructors
	TCon tc
	 ->	newNode src (tyConKind tc)
			$ NCon tc
			
	-- Variables.
 	TVar k (UVar v)
	 -> 	ensureClassWithVar k src v 

	TVar _ (UClass cid)
	 ->	sinkClassId cid
		
	_  -> panic stage
		( "feedType: cannot feed this type into the graph.\n"
		% "   type        = " % tt 	% "\n"
		% "   source      = " % src 	% "\n")


-- | Add a node to the type graph, and activate the corresponding class.
addNode :: ClassId -> TypeSource -> Kind -> Node -> SquidM ClassId
addNode cid src kind node
 = do	addNodeToClass cid kind src node
	activateClass cid
	return cid


-- | Create a new class in the graph, with the given node.
newNode :: TypeSource -> Kind -> Node -> SquidM ClassId
newNode src kind node
 = do	cid	<- allocClass kind src
	addNode cid src kind node
	return cid
	

-- Fetters ----------------------------------------------------------------------------------------
feedFetter 
	:: TypeSource
	-> Fetter 
	-> SquidM ()

feedFetter src f
 = trace ("feedFetter " % f % "\n") >>
   case f of
	FWhere t1 t2
	 -> do	cid1	<- feedType src t1
	 	cid2	<- feedType src t2
		mergeClasses [cid1, cid2]
		return ()

	FMore t1 t2	
	 -> 	feedFetter src (FWhere t1 t2)

	FConstraint{}
	 -> do	addFetter src f
	 	return ()

	FProj pj v tDict tBind
	 -> do 	[cidDict', cidBind'] <- mapM (feedType src) [tDict, tBind]
		let kDict	= kindOfType tDict
		let kBind	= kindOfType tBind

		addFetter src 
		 $ FProj pj v 
			(TVar kDict $ UClass cidDict')
			(TVar kBind $ UClass cidBind')

		return ()
		

-- | Add a new fetter constraint to the graph
addFetter
	:: TypeSource
	-> Fetter
	-> SquidM Bool	-- ^ Whether the fetter we added was new.
	
-- Single parameter type class contraints are added directly to the equivalence
--	class which they constrain.
addFetter src f@(FConstraint vFetter [t])
 = do	
	trace	$ "    * addFetter: " % f	% "\n"

	-- The target type t could be a TVar or a TClass
	--	Use feedType to make sure it's a TClass
	cid		<- feedType src t
	
	-- check what fetters are already there
	Just cls@Class{} <- lookupClass cid
	
	case Map.lookup vFetter (classFetters cls) of

	 -- We already had a fetter of this sort on the class.
	 Just{}
	  -> 	return False

	 -- This is a new fetter. 
	 Nothing
	  -> do	modifyClass cid $ \c -> c {
			classFetters = Map.insertWith (Seq.><) vFetter 
					(Seq.singleton src) (classFetters c) }
		
		activateClass cid
		return True
							
	
-- Multi parameter type class constraints are added as ClassFetter nodes in the graph 
--	and the equivalence classes which they constraint hold ClassIds which point to them.
addFetter src (FConstraint v ts)
 = do 	
 	-- create a new class to hold this node
	cidF		<- allocClass KNil src
	 	
	-- add the type args to the graph
	cids		<- mapM (feedType src) ts
	let kinds	= map kindOfType ts
	let ts'		= zipWith (\k c -> TVar k $ UClass c) kinds cids
	
	-- add the fetter to the graph
	let f	= FConstraint v ts'

	updateClass True cidF
	 $ ClassFetter
	 	{ classId	= cidF
		, classFetter	= f 
		, classSource	= src }

	-- work out what cids this constraint is acting on
	let cids'	= map (\tt -> let TVar _ (UClass cid) = tt in cid) ts'
	
	-- add a reference to this constraint to all those classes
	mapM	(\cid -> modifyClass cid
			$ \c -> c { classFettersMulti = Set.insert cidF (classFettersMulti c) })
		$ cids'

	return True

addFetter src (FProj j v1 tDict tBind)
 = do
 	-- a new class to hold this node
 	cidF	<- allocClass KNil src
	
	-- add the type args to the graph
 	[cidDict', cidBind'] <- mapM (feedType src) [tDict, tBind]
	let kDict	= kindOfType tDict
	let kBind	= kindOfType tBind

	-- add the fetter to the graph
	let f	= FProj j v1 
			(TVar kDict $ UClass cidDict')
			(TVar kBind $ UClass cidBind')
	
	updateClass True cidF
	 $ ClassFetter
	 	{ classId	= cidF
		, classFetter	= f 
		, classSource	= src }
			
	-- add a reference to the constraint to all those classes it is acting on
	mapM	(\cid -> modifyClass cid
			$ \c -> c { classFettersMulti = Set.insert cidF (classFettersMulti c) })
		[cidDict', cidBind']
			
	return True

addFetter _ _
	= panic stage $ "addFetter: no match"

