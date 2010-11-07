{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}

module DDC.Solve.Crush.Unify
	( crushUnifyInClass
	, addErrorConflict 
	, isShallowConflict)
where
import DDC.Main.Error
import DDC.Solve.Error
import DDC.Solve.Trace
import DDC.Solve.State
import DDC.Type
import Util
import qualified Data.Set	as Set

debug	= False
stage	= "Type.Crush.Unify"
trace s	= when debug $ traceM s
traceln s = trace (s % "\n")

-- | Unify the elements in an equivalences classes queue.
--   If there are any errors then these are added to the SquidM monad.
--   Returns `True` if the class was modified.
crushUnifyInClass 
	:: ClassId	
	-> SquidM Bool
	
crushUnifyInClass cid
 = do	trace	 $ "-- Crush.unify " % cid % "\n"	
	Just cls <- lookupClass cid
	
	case cls of
	 Class	{ classKind = kind }

		-- TODO: make sure the grinder shouldn't be calling us on these kind of classes,
		--       and panic if it does.
		| isRegionKind kind || isEffectKind kind || isClosureKind kind
		-> do	trace $ ppr "   -- boring class\n\n"
			return False
		
		-- The class is already unified.
		| Just _	<- classUnified cls
		-> do	trace $ ppr "   -- already unified\n\n"
			return False
	
		-- Do some unification.
		| otherwise
		-> crushUnifyInClass_unify cid cls

	 ClassFetter{}
	  -> do	trace $ ppr "   -- is fetter class\n\n"
		return False

	 ClassFetterDeleted{}
	  -> do	trace $ ppr "   -- WARNING: not unifying deleted fetter\n\n"
		return False


-- Ensure that there are no NBot nodes in the queue, as they don't contribute
-- to the type. For types of non-injective kind there shouldn't be NVars either.
crushUnifyInClass_unify cid cls@Class{}
 = let	badNodes
		| isInjectiveKind (classKind cls)
		= filter (isNBot . fst) $ classTypeSources cls
	 
		| otherwise
		= filter (\(n, _) -> isNBot n || isNVar n)
		$ classTypeSources cls
	
   in	case badNodes of
	 []	-> crushUnifyInClass_checked cid cls (classTypeSources cls)
	 _	-> panic stage $ "crushUnifyInClass_unify: bad node"
	

crushUnifyInClass_checked cid cls@Class{} queue
 = do	
	traceln	$ "    kind:        " % classKind cls
	traceln	$ "    queue after:\n" 
		% indent (vcat $ map ppr queue)

	-- If there is nothing left in the queue, or there's only one element
	-- then we're done. Otherwise call the real unifier.
	node_final <- case queue of
			[] 		-> return	$ nBot
			[(n, _)] 	-> return	$ n
			_  		-> crushUnifyInClass_merge cid cls queue
			
  	trace	$ "    node_final  = " % node_final	% "\n\n"

	-- Update the class with the new type
  	updateClass False cid cls 
		{ classUnified 	= Just node_final }

	-- Wake up any MPTCs acting on this class
	mapM activateClass 
		$ Set.toList 
		$ classFettersMulti cls

	return True

				
-- Merge a list of types, adding new constraints to the graph if needed.
--	this does the actual unification.
crushUnifyInClass_merge cid cls@Class{} queue@((n1, _):_)
	
	-- All constructors in the queue must be identical.
	| NCon{}	<- n1 
	, all (\x -> fst x == n1) (tail queue)
	= 	return n1

	-- Unify the left and right of type applications.
	| NApp{}	<- n1
	, Just (cid1s, cid2s)
		<- liftM unzip $ sequence
		$ map (\(n, _) -> case n of
				NApp cid1 cid2 	-> Just (cid1, cid2)
				_		-> Nothing)
		$ queue	
	= do
		cid1'	<- mergeClasses cid1s
		cid2'	<- mergeClasses cid2s
		return	$ NApp cid1' cid2'

	-- If the none of the previous rules matched then we've got a type error.
	| otherwise
 	= do	addErrorConflict cid cls
		return NError


-- | Given a class known to have a unification problem, work out 
--	what went wrong and add an error to the `SquidM` monad.
addErrorConflict :: ClassId -> Class -> SquidM ()
addErrorConflict  cid cls
 = do	
	-- Filter out vars, as they don't conflict with anything
 	let tsCtorsNode
		= filter (\(t, _) -> not $ isNVar t)
		$ classTypeSources cls
 
	-- Gather up pairs of nodes that conflict.
 	let conflicts
		= [ (n1, n2)
			| n1@(t1, _)	<- tsCtorsNode
			, n2@(t2, _)	<- tsCtorsNode
			, isShallowConflict t1 t2 ]
	
	-- We could perhaps do some more involved analysis, 
	--	but just report the first pair of conflicts.
	case takeHead conflicts of
	 Just conf	-> addErrorConflict' cid cls conf

	-- Sanity, check that we've actually identified the problem and added
	--	an error to the state.
	 _ -> panic stage
		 $ "errorConflict: Couldn't identify the error in class " % cid % "\n"
		 % "   unified: \n"	%> classUnified cls 			% "\n\n"
		 % "   nodes:\n"	%> ("\n" %!% classTypeSources cls) 	% "\n\n"


-- Add an error recording that these two types conflict.
addErrorConflict' cid cls ((node1, src1), (node2, src2))
 = do	type1	<- getTypeOfNodeAsSquid (classKind cls) node1
	type2	<- getTypeOfNodeAsSquid (classKind cls) node2
	
	-- add an error to the error list.
	addErrors 
		[ ErrorUnifyCtorMismatch 
			{ eCtor1	= type1
			, eTypeSource1	= src1
			, eCtor2	= type2
			, eTypeSource2	= src2 }]

	-- mark the class in the graph that contains the error.
	updateClass False cid
		cls { classUnified	= Just $ NError }


-- | Check whether two types conflict on their outermost constructor.
isShallowConflict :: Node -> Node -> Bool
isShallowConflict t1 t2
 = case (t1, t2) of
	(NApp{},   NApp{})	-> False
	(NCon tc1, NCon tc2)	-> not $ tc1 == tc2
	(NVar{},   _)		-> False
	(_,        NVar{})	-> False
	_			-> True
	
