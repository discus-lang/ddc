{-# OPTIONS -fno-warn-incomplete-record-updates #-}

-- | Unify multiple types in an equivalence class.
module Type.Crush.Unify
	( crushUnifyClass 
	, isShallowConflict 
	, addErrorConflict )
where
import Type.Location
import Type.Exp
import Type.Error
import Type.State
import Type.Util
import Type.Class
import Type.Feed
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
				TVar{}	-> False
				TBot{}	-> False
				_	-> True)
			((maybeToList $ classType c) ++ classQueue c)

	trace	$ "    queue_clean  = " % show queue_clean % "\n"

	-- If there is nothing left in the queue, or there's only one element
	--	then we're done. Otherwise call the reall unifier.
	t_final	<- case queue_clean of
			[] 	-> return	$ TBot (classKind c)
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
	
	-- class ids
	| TClass k _	<- t
	= do
		let cids	=  map (\(TClass k cid) -> cid) queue
		cid		<- mergeClasses cids
		return	$ TClass k cid

	-- all constructors in the queue have to be the same
	| TCon{}	<- t 
	, all (== t) (tail queue)
	= 	return t

	-- unify the left and right of type applications.
	| TApp t1 t2	<- t
	, Just (t1s, t2s)
		<- liftM unzip $ sequence
		$ map (\x -> case x of
				TApp t1 t2 	-> Just (t1, t2)
				_		-> Nothing)
		$ queue

	, k1s	<- map (\(Just k) -> k) $ map kindOfType t1s
	, [_]	<- nub k1s
	
	, k2s	<- map (\(Just k) -> k) $ map kindOfType t2s
	, [_]	<- nub k2s
	
	= do
		let ?src	= TSNil "Type.Crush.Unify/TApp"
		Just t1s_fed	<- liftM sequence $ mapM (feedType Nothing) t1s
		Just t2s_fed 	<- liftM sequence $ mapM (feedType Nothing) t2s

		t1' <- mergeClassesT t1s_fed
		t2' <- mergeClassesT t2s_fed
		
		return	$ TApp t1' t2'

	-- From the effect weakening rule it's always return a larger effect than needed.
	-- Therefore, to "unify" two effects we want take their l.u.b and use that in place of both.
	| and $ map (\t -> kindOfType_orDie t == kEffect) queue
	= do	return	$ makeTSum kEffect queue

	-- .. likewise for closures
	| and $ map (\t -> kindOfType_orDie t == kClosure) queue
	= do	return	$ makeTSum kClosure queue

	-- if none of the previous rules matched then we've got a type error in the graph.
	| otherwise
 	= do	addErrorConflict cidT c 
		return $ TError (kindOfType_orDie t) 
				(TypeErrorUnify $ classQueue c)


-- | This function is called by unifyClassCheck when it finds a problem
-- 	in the graph. We diagnose the problem, add an error message to 
-- 	the SquidM monad and mark the class as containing an error.
--
addErrorConflict :: ClassId -> Class -> SquidM ()
addErrorConflict  cid c
 = do	
	-- filter out TVars, as they don't conflict with anything
 	let tsCtorsNode
		= filter (\(t, _) -> not $ isSomeTVar t)
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
 = do	
	-- add an error to the error list.
	addErrors 
		[ ErrorUnifyCtorMismatch 
			{ eCtor1	= t1
			, eTypeSource1	= s1
			, eCtor2	= t2
			, eTypeSource2	= s2}]

	-- mark the class in the graph that contains the error.
	updateClass cid
		c { classType	= Just $ TError (classKind c) 
						(TypeErrorUnify $ classQueue c) }

	return ()


-- | check whether two types conflict on their outermost constructor.
isShallowConflict :: Type -> Type -> Bool
isShallowConflict t1 t2
	| TApp t11 t12	<- t1	
	, TApp t21 t22	<- t2	
	, Just k11	<- kindOfType t11
	, Just k12	<- kindOfType t12
	, Just k21	<- kindOfType t21
	, Just k22	<- kindOfType t22
	, k11 == k21
	, k12 == k22
	= False
	
	| TCon tc1	<- t1
	, TCon tc2	<- t2
	= not $ tc1 == tc2
	
	| TVar k1 _	<- t1	
	, Just k2	<- kindOfType t2
	, k1 == k2
	= False

	| TVar k2 _	<- t2
	, Just k1	<- kindOfType t1
	, k1 == k2
	= False
		
	| otherwise
	= True
	
	