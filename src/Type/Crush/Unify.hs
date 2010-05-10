{-# OPTIONS -fno-warn-incomplete-record-updates #-}

-- | Unify multiple types in an equivalence class.
module Type.Crush.Unify
	( crushUnifyClass 
	, isShallowConflict 
	, addErrorConflict )
where
-- import Type.Location
import Type.Exp
-- import Type.Builtin
-- import Type.Error
import Type.State
-- import Type.Util
import Type.Class
-- import Type.Feed
import Util
import DDC.Main.Error
import qualified Data.Set	as Set

-----
debug	= False
stage	= "Type.Crush.Unify"
trace s	= when debug $ traceM s


-- | Unify the elements in an equivalences classes queue.
--   If there are any errors then these are added to the SquidM monad.
--
crushUnifyClass 
	:: ClassId	
	-> SquidM Bool	-- whether this class was unified

crushUnifyClass	cidT
 = do	Just c	<- lookupClass cidT
	crushUnifyClass_cls cidT c

	
-- | check whether the work has alreay been done
crushUnifyClass_cls cidT (ClassForward cid')
	= crushUnifyClass cid'

crushUnifyClass_cls cidT c@Class{}

	-- class is already unified
	| []		<- classQueue c 
	, Just t	<- classType c
	= return False
	
	-- do some unification
	| otherwise
	= crushUnifyClass_unify cidT c

crushUnifyClass_cls cidT c
	= return False
	
	
-- | sort through the ctors in the queue, and update the class.
crushUnifyClass_unify cidT c
 = do	trace	$ "*   Unify.unifyClass " % cidT % "\n"
		% "    type         = " % classType c	% "\n"
		% "    name         = " % className c	% "\n"
		% "    nodes        = " % classTypeSources c % "\n\n"

	let queue_type
		= (maybeToList $ classType c) ++ classQueue c

	trace	$ "    queue+type   = " % show queue_type % "\n"

 	-- crush out nested unifiers and filter out vars and bottoms as they don't 
	--	contribute to the constructor
	let queue_clean	
		= filter 
			(\x -> case x of
				NVar{}		-> False
				NBot		-> False
				_		-> True)
			((maybeToList $ classType c) ++ classQueue c)

	trace	$ "    queue_clean  = " % show queue_clean % "\n"

	-- If there is nothing left in the queue, or there's only one element
	--	then we're done. Otherwise call the reall unifier.
	t_final	<- case queue_clean of
			[] 	-> return	$ NBot
			[t] 	-> return	$ t
			_  	-> crushUnifyClass_merge cidT c queue_clean
			
  	trace	$ "    t_final      = " % t_final	% "\n\n"

	-- Update the class with the new type
  	updateClass cidT c 
		{ classType 	= Just t_final
		, classQueue	= [] }

	-- Wake up any MPTCs acting on this class
	mapM activateClass $ Set.toList $ classFettersMulti c

	return True

				
-- | merge a list of types, adding new constraints to the graph if needed.
--	this does the actual unification.
crushUnifyClass_merge cidT c queue@(t:_)
	
	-- all constructors in the queue have to be the same
	| NCon{}	<- t 
	, all (== t) (tail queue)
	= 	return t

	-- unify the left and right of type applications.
	| NApp{}	<- t
	, Just (cid1s, cid2s)
		<- liftM unzip $ sequence
		$ map (\x -> case x of
				NApp cid1 cid2 	-> Just (cid1, cid2)
				_		-> Nothing)
		$ queue
	
	= do
		cid1' <- mergeClasses cid1s
		cid2' <- mergeClasses cid2s
		return	$ NApp cid1' cid2'

	-- From the effect weakening rule it's always return a larger effect than needed.
	-- Therefore, to "unify" two effects we want take their l.u.b and use that in place of both.
	| NSum{}	<- t
	= panic stage $ "crushUnifyClass_merge: makeNSum\n"
	--	return	$ makeNSum queue

	-- if none of the previous rules matched then we've got a type error in the graph.
	| otherwise
 	= panic stage $ "crushUnifyClass_merge: type error\n"
{-		addErrorConflict cidT c 
		return $ TError (kindOfType_orDie t) 
				(TypeErrorUnify $ classQueue c)
-}

-- | This function is called by unifyClassCheck when it finds a problem
-- 	in the graph. We diagnose the problem, add an error message to 
-- 	the SquidM monad and mark the class as containing an error.
--
addErrorConflict :: ClassId -> Class -> SquidM ()
addErrorConflict  cid c
 = do	
	-- filter out TVars, as they don't conflict with anything
 	let tsCtorsNode
		= filter (\(t, _) -> not $ isNVar t)
		$ classTypeSources c
 
	-- gather up pairs of nodes that conflict.
 	let conflicts
		= [ (n1, n2)
			| n1@(t1, _)	<- tsCtorsNode
			, n2@(t2, _)	<- tsCtorsNode
			, isShallowConflict t1 t2 ]
	
	-- We could perhaps do some more involved analysis, 
	--	but just report the first pair of conflicts.
	case takeHead conflicts of
	 Just conf	-> addErrorConflict' cid c conf

	-- sanity, check that we've actually identified the problem and added
	--	an error to the state.
	 _ -> panic stage
		 $ "errorConflict: Couldn't identify the error in class " % cid % "\n"
		 % "   type: \n"  %> classType c 			% "\n\n"
		 % "   queue: \n" %> classQueue c	 		% "\n\n"
		 % "   nodes:\n"  %> ("\n" %!% classTypeSources c) 	% "\n\n"


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
	
	