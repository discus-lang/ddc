{-# OPTIONS -fno-warn-incomplete-record-updates #-}

-- | Unify multiple types in an equivalence class.
module Type.Crush.Unify
	( crushUnifyInClass
	, isShallowConflict 
	, addErrorConflict )
where
import Type.Exp
import Type.Builtin
import Type.State
import Type.Class
import Util
import DDC.Main.Error
import qualified Data.Set	as Set
-- import Type.Location
-- import Type.Builtin
-- import Type.Error
-- import Type.Util
-- import Type.Feed

debug	= False
stage	= "Type.Crush.Unify"
trace s	= when debug $ traceM s


-- | Unify the elements in an equivalences classes queue.
--   If there are any errors then these are added to the SquidM monad.
crushUnifyInClass 
	:: ClassId	
	-> SquidM Bool	-- whether this class was unified

crushUnifyInClass cid
 = do	Just cls	<- lookupClass cid
	case cls of
	 ClassForward _ cid'
	  -> crushUnifyInClass cid'
	
	 Class{}
		-- class is already unified
		| Just t	<- classType cls
		-> return False
	
		-- do some unification
		| otherwise
		-> crushUnifyInClass_unify cid cls

	 _ -> return False	
	
	
-- | sort through the ctors in the queue, and update the class.
crushUnifyInClass_unify cid cls@Class{}
 = do	trace	$ "--  unifyClass " % cid % "\n"
		% "    type         = " % classType cls	% "\n"
		% "    name         = " % className cls	% "\n"
		% "    nodes        = " % classTypeSources cls % "\n\n"

	-- Disregard vars and bottoms as they don't contribute real type constraints.
	-- TODO: perhaps better to store these separtely so we don't have to keep
	--	 filtering them out.
	let queue
		= filter (\(n, src) -> not (isNVar n || isNBot n))
		$ classTypeSources cls

	trace	$ "    queue       = " % show queue % "\n"

	-- If there is nothing left in the queue, or there's only one element
	--	then we're done. Otherwise call the real unifier.
	node_final <- case queue of
			[] 		-> return	$ nBot
			[(n, _)] 	-> return	$ n
			_  		-> crushUnifyInClass_merge cid cls queue
			
  	trace	$ "    node_final    = " % node_final	% "\n\n"

	-- Update the class with the new type
  	updateClass cid cls 
		{ classType 	= Just node_final }

	-- Wake up any MPTCs acting on this class
	mapM activateClass 
		$ Set.toList 
		$ classFettersMulti cls

	return True

				
-- | merge a list of types, adding new constraints to the graph if needed.
--	this does the actual unification.
crushUnifyInClass_merge cid cls@Class{} queue@((n1, _):_)
	
	-- all constructors in the queue have to be the same
	| NCon{}	<- n1 
	, all (\x -> fst x == n1) (tail queue)
	= 	return n1

	-- unify the left and right of type applications.
	| NApp{}	<- n1
	, Just (cid1s, cid2s)
		<- liftM unzip $ sequence
		$ map (\(n, _) -> case n of
				NApp cid1 cid2 	-> Just (cid1, cid2)
				_		-> Nothing)
		$ queue	
	= do
		cid1' <- mergeClasses cid1s
		cid2' <- mergeClasses cid2s
		return	$ NApp cid1' cid2'

	-- For effects and closures, if all of the nodes to be unified are already sums
	-- then we can just make a new one containing all the cids. Otherwise, we have to
	-- split out the non-sums into their own classes.
	| classKind cls == kEffect || classKind cls == kClosure
	= do	let (ncSums, ncOthers) = partition (isNSum . fst) queue
		let Just cidsSums =  liftM Set.unions   $ sequence $ map (takeNSum . fst) $ ncSums
		cidsOthers	  <- liftM Set.fromList $ mapM (pushIntoNewClass (classKind cls)) ncOthers
		return	$ makeNSum $ Set.union cidsSums cidsOthers
	

	-- if none of the previous rules matched then we've got a type error in the graph.
	| otherwise
 	= do	panic stage $ vcat
			[ ppr "crushUnifyClass_merge: type error\n"
			, ppr queue ]
{-		addErrorConflict cidT c 
		return $ TError (kindOfType_orDie t) 
				(TypeErrorUnify $ classQueue c)
-}

pushIntoNewClass kind (node, src)
 = do	cid	<- allocClass kind 
	addToClass cid src kind node
	return cid
	


-- | This function is called by unifyClassCheck when it finds a problem
-- 	in the graph. We diagnose the problem, add an error message to 
-- 	the SquidM monad and mark the class as containing an error.
--
addErrorConflict :: ClassId -> Class -> SquidM ()
addErrorConflict  cid cls
 = do	
	-- filter out TVars, as they don't conflict with anything
 	let tsCtorsNode
		= filter (\(t, _) -> not $ isNVar t)
		$ classTypeSources cls
 
	-- gather up pairs of nodes that conflict.
 	let conflicts
		= [ (n1, n2)
			| n1@(t1, _)	<- tsCtorsNode
			, n2@(t2, _)	<- tsCtorsNode
			, isShallowConflict t1 t2 ]
	
	-- We could perhaps do some more involved analysis, 
	--	but just report the first pair of conflicts.
	case takeHead conflicts of
	 Just conf	-> addErrorConflict' cid cls conf

	-- sanity, check that we've actually identified the problem and added
	--	an error to the state.
	 _ -> panic stage
		 $ "errorConflict: Couldn't identify the error in class " % cid % "\n"
		 % "   type: \n"  %> classType cls 			% "\n\n"
		 % "   nodes:\n"  %> ("\n" %!% classTypeSources cls) 	% "\n\n"


-- | add an error recording that these two types conflict.
addErrorConflict' cid c ((t1, s1), (t2, s2))
 = panic stage $ "addErrorConflict'"
	-- add an error to the error list.
{-	addErrors 
		[ ErrorUnifyCtorMismatch 
			{ eCtor1	= t1
			, eTypeSource1	= s1
			, eCtor2	= t2
			, eTypeSource2	= s2}]

	-- mark the class in the graph that contains the error.
	updateClass cid
		c { classType	= Just $ NError }

	return ()
-}

-- | check whether two types conflict on their outermost constructor.
isShallowConflict :: Node -> Node -> Bool
isShallowConflict t1 t2
	| NApp{}	<- t1	
	, NApp{}	<- t2	
	= False
	
	| NCon tc1	<- t1
	, NCon tc2	<- t2
	= not $ tc1 == tc2
	
	| NVar{}	<- t1	
	= False

	| NVar{}	<- t2
	= False
		
	| otherwise
	= True
	
	