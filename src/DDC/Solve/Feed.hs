{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# OPTIONS -fno-warn-incomplete-record-updates #-}

-- | ''Feeding'' refers to adding type constraints to the type graph.
module DDC.Solve.Feed
	( feedConstraint
	, feedType 
	, feedFetter
	, addFetter)
where
import Constraint.Exp
import Type.Link
import Type.Dump		()
import Util
import DDC.Solve.Interface.Problem
import DDC.Solve.Location
import DDC.Solve.State
import DDC.Main.Error
import DDC.Type
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
		-- Lookup/create new class for the var on the left
		-- There might already be a class for this var in the graph.
		-- If not then create a new one.
	 	cid1	<- ensureClassWithVar k src v1

		-- feed the RHS into the graph.
		cid2	<- feedType src t2

		-- merge left and right hand sides.
		mergeClasses [cid1, cid2]
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
	 	cidT		<- feedType src t2

		return cidT


	-- For empty classes ther are no nodes, but create the class anyway so we have its cid
	TSum k []
	 -> do	cidT		<- allocClass k src
		addNode cidT src k $ NBot
		return cidT

	-- Add summations
	-- TODO: all the nodes should be stored seprately.
	TSum k ts
	 -> do 	cidT		<- allocClass k src
		cids		<- mapM (feedType src) ts

		-- TODO: Don't add sums to classes.
		addNode cidT src k 
			$ NSum (Set.fromList cids)

		return cidT

	-- TFree's that we get from the constraints might refer to types that are in
	-- the defs table but not the graph. We don't want to pollute the graph
	-- with the whole external def so trim these types down and just add the
	-- closure information that we need.
	TApp{}
	 | Just (v1, t@(TVar kV (UVar v2)))	<- takeTFree tt
	 , kV == kValue
	 -> do	cid		<- allocClass kClosure src
		env		<- gets stateEnv
		case Map.lookup v2 (squidEnvDefs env) of
		 -- type that we're refering to is in the defs table
		 Just (ProbDef _ _ tDef)
		  -> do	let tDef_trim	= trimClosureT $ flattenT tDef

		  	tDef'		<- linkType [] src tDef_trim

			-- we use addToClass here because we don't need to register effects and 
			--	classes for crushing in this type
			addNodeToClass cid kClosure src
				$ NFree v1 tDef'

			return cid

		 -- type must be in the graph
		 Nothing
		  -> do	t'		<- linkType [] src t

			addNode	cid src	kClosure
				$ NFree v1 t'

			return cid

	-- A closure term that contains a type with constructors instead of just a plain
	-- variable. We allow these in signatures, but we only need the trimmed
	-- version for inference.
	-- TODO: trim the closure.
	TApp{}
	 | Just (v1, t)	<- takeTFree tt
	 -> do	cid	<- allocClass kClosure src
		t'	<- linkType [] src t

		addNode	cid src kClosure	
			$ NFree v1 t'

		return cid

	-- Type applications.
	TApp t1 t2
	 -> do	let k	= kindOfType tt
	 	cidT	<- allocClass k src
	 	cid1	<- feedType src t1
		cid2	<- feedType src t2

		addNode cidT src k
			$ NApp cid1 cid2

		return cidT

	-- Plain constructors
	TCon tc
	 -> do	let k	= tyConKind tc
		cidT	<- allocClass k src

	 	addNode cidT src k 
			$ NCon tc

		return cidT

	-- Variables.
 	TVar k (UVar v)
	 -> do 	cidT		<- ensureClassWithVar k src v 
		return cidT


	TVar _ (UClass cid)
	 -> do 	cidT'		<- sinkClassId cid
		return cidT'
		
	_  -> panic stage
		( "feedType: cannot feed this type into the graph.\n"
		% "   type        = " % tt 	% "\n"
		% "   source      = " % src 	% "\n")


-- | Add a node to the type graph, and activate the corresponding class.
addNode :: ClassId
	-> TypeSource
	-> Kind
	-> Node
	-> SquidM ()
	
addNode cid src kind node
 = do	addNodeToClass cid kind src node
	activateClass cid


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
	-> SquidM Bool
	
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

	modifyClass cidF
	 $ \_ -> ClassFetter
	 	{ classId	= cidF
		, classFetter	= f 
		, classSource	= src }

	activateClass cidF

	-- work out what cids this constraint is acting on
	let cids'	= map (\(TVar _ (UClass cid)) -> cid) ts'
	
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
	
	modifyClass cidF
	 $ \_ -> ClassFetter
	 	{ classId	= cidF
		, classFetter	= f 
		, classSource	= src }
		
	activateClass cidF
	
	-- add a reference to the constraint to all those classes it is acting on
	mapM	(\cid -> modifyClass cid
			$ \c -> c { classFettersMulti = Set.insert cidF (classFettersMulti c) })
		[cidDict', cidBind']
			
	return True

addFetter _ _
	= panic stage $ "addFetter: no match"

