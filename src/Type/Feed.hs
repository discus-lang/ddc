{-# OPTIONS -fno-warn-incomplete-record-updates #-}

module Type.Feed
	( feedConstraint
	, feedType 
	, feedFetter
	, addFetter)
where
import Constraint.Exp
import Type.Exp
import Type.Builtin
import Type.Location
import Type.State
import Type.Class
import Type.Util
import Type.Link
import Type.Dump		()
import Util
import DDC.Main.Error
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import qualified Data.Sequence	as Seq

stage	= "Type.Feed"

debug	= False
trace ss
 = when debug $ traceM ss

-- feedConstraint ----------------------------------------------------------------------------------
-- | Add a constraint to the type graph.
feedConstraint :: CTree -> SquidM ()
feedConstraint cc
 = trace ("feedConstraint: " % cc % "\n") >>
   case cc of
	-- Equality constraints. The LHS must be a variable.
 	CEq src (TVar k v1) t2
	 -> do
		-- create a new class for the LHS, with just that var in it.
	 	cid1	<- makeClassFromVar src k v1

		-- feed the RHS into the graph.
		cid2	<- feedType src t2

		-- merge left and right hand sides.
		mergeClasses [cid1, cid2]
		return ()

	CEqs src ts
	 -> do	cids		<- mapM (feedType src) ts
		mergeClasses cids
		return ()


	-- Signatures behave the same way as plain equality constraints.
	CSig src t1@(TVar k v1) t2
	 -> do 	feedConstraint (CEq src t1 t2)
		return ()

	-- Class constraints
	CClass src vC ts
	 -> do	addFetter src (FConstraint vC ts)
		return ()

	-- Projection constraints.
	CProject src j v1 tDict tBind
	 -> do	addFetter src (FProj j v1 tDict tBind)
		return ()		
				
	-- Type definitions, eg data constructors, external functions.
	--	These get added to the defs table by the main solver,
	--	and shouldn't appear in the graph.
	CDef src (TVar k v1) t2
	 -> 	panic stage 
		 $ "feedConstraint: not feeding def " % cc % "\n"

	_ -> 	panic stage
		 $ "feedConstraint: can't feed " % cc % "\n"


-- feedType --------------------------------------------------------------------------------------------
-- | Add a type to the type graph.
--	The type is converted to the node form on the way in.
feedType 	
	:: TypeSource
	-> Type 
	-> SquidM ClassId

feedType src tt
 = case tt of
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
				
		mapM_ (feedFetter src) fs2
	 	t3		<- feedType src t2
		return	t3

	TConstrain{}
	 -> feedType src
	 $  toFetterFormT tt

	TSum k ts
	 | []	<- ts
	 -> do	cidT		<- allocClass k
		addNode cidT src k $ NBot
		return cidT

	 | otherwise
	 -> do 	cidT		<- allocClass k
		cids		<- mapM (feedType src) ts
		addNode cidT src k 
			$ NSum (Set.fromList cids)

		return cidT

	TApp t1 t2
	 -> do	
		let Just k	= kindOfType tt
	 	cidT		<- allocClass k
	 	cid1		<- feedType src t1
		cid2		<- feedType src t2

		addNode cidT src k	
			$ NApp cid1 cid2

		return cidT

	TCon tc
	 -> do	let k		= tyConKind tc
		cidT		<- allocClass k

	 	addNode cidT src k 
			$ NCon tc

		return cidT

 	TVar k v 
	 -> do 	cidT		<- makeClassFromVar src k v 
		return cidT

	-- closure
	-- TFree's that we get from the constraints might refer to types that are in
	--	the defs table but not the graph. We don't want to pollute the graph
	--	with the whole external def so trim these types down and just add the
	--	closure information that we need.
	TFree v1 t@(TVar kV v2)
	 | kV == kValue
	 -> do	cid		<- allocClass kClosure
		defs		<- gets stateDefs
		case Map.lookup v2 defs of
		 -- type that we're refering to is in the defs table
		 Just tDef
		  -> do	let tDef_trim	= toFetterFormT 
					$ trimClosureT_constrainForm Set.empty Set.empty 
					$ toConstrainFormT
					$ flattenT tDef

		  	tDef'		<- linkType [] src tDef_trim

			-- we use addToClass here because we don't need to register effects and 
			--	classes for crushing in this type
			addToClass cid src kClosure 
				$ NFree v1 tDef'

			return cid

		 -- type must be in the graph
		 Nothing
		  -> do	t'		<- linkType [] src t

			addNode	cid src	kClosure
				$ NFree v1 t'

			return cid

	-- A non-var closure. We can get these from signatures and instantiated schemes
	TFree v1 t
	 -> do	cid		<- allocClass kClosure
		t'		<- linkType [] src t

		addNode	cid src kClosure	
			$ NFree v1 t'

		return cid

	TClass k cid
	 -> do 	cidT'		<- sinkClassId cid
		return cidT'
		
	_  -> panic stage
		( "feedType: cannot feed this type into the graph.\n"
		% "   type    = " % tt 		% "\n"
		% "   source  = " % src 	% "\n")


-- | Add a node to the type graph, and activate the corresponding class.
addNode :: ClassId
	-> TypeSource
	-> Kind
	-> Node
	-> SquidM ()
	
addNode cid src kind node
 = do	addToClass cid src kind node
	activateClass cid


-- Fetter ------------------------------------------------------------------------------------------
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

	FConstraint v ts
	 -> do	addFetter src f
	 	return ()

	FProj pj v tDict tBind
	 -> do 	[cidDict', cidBind'] <- mapM (feedType src) [tDict, tBind]
		let Just kDict	= kindOfType tDict
		let Just kBind	= kindOfType tBind

		addFetter src 
		 $ FProj pj v 
			(TClass kDict cidDict')
			(TClass kBind cidBind')

		return ()
		

-- addFetter ---------------------------------------------------------------------------------------
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

	 -- We already had a fetter of this sort on the class, but we add 
	 -- the source info anyway to help with error reporting.
	 Just srcs	
	  -> do	modifyClass cid $ \c -> c {
			classFetters = Map.insertWith (Seq.><) vFetter 
					(Seq.singleton src) (classFetters c) }
		return False
		
	 -- This is a new fetter. 
	 Nothing
	  -> do	modifyClass cid $ \c -> c {
			classFetters = Map.singleton vFetter (Seq.singleton src) }
		activateClass cid
		return True
							
	
-- Multi parameter type class constraints are added as ClassFetter nodes in the graph 
--	and the equivalence classes which they constraint hold ClassIds which point to them.
addFetter src f@(FConstraint v ts)
 = do 	
 	-- create a new class to hold this node
	cidF		<- allocClass KNil
	 	
	-- add the type args to the graph
	cids		<- mapM (feedType src) ts
	let Just kinds	= sequence $ map kindOfType ts
	let ts'		= zipWith TClass kinds cids
	
	-- add the fetter to the graph
	let f	= FConstraint v ts'

	modifyClass cidF
	 $ \c -> ClassFetter
	 	{ classId	= cidF
		, classFetter	= f 
		, classSource	= src }

	activateClass cidF

	-- work out what cids this constraint is acting on
	let cids	= map (\(TClass k cid) -> cid) ts'
	
	-- add a reference to this constraint to all those classes
	mapM	(\cid -> modifyClass cid
			$ \c -> c { classFettersMulti = Set.insert cidF (classFettersMulti c) })
		$ cids

	return True

addFetter src f@(FProj j v1 tDict tBind)
 = do
 	-- a new class to hold this node
 	cidF	<- allocClass KNil
	
	-- add the type args to the graph
 	[cidDict', cidBind'] <- mapM (feedType src) [tDict, tBind]
	let Just kDict	= kindOfType tDict
	let Just kBind	= kindOfType tBind

	-- add the fetter to the graph
	let f	= FProj j v1 
			(TClass kDict cidDict')
			(TClass kBind cidBind')
	
	modifyClass cidF
	 $ \c -> ClassFetter
	 	{ classId	= cidF
		, classFetter	= f 
		, classSource	= src }
		
	activateClass cidF
	
	-- add a reference to the constraint to all those classes it is acting on
	mapM	(\cid -> modifyClass cid
			$ \c -> c { classFettersMulti = Set.insert cidF (classFettersMulti c) })
		[cidDict', cidBind']
			
	return True
	

