{-# OPTIONS -fno-warn-incomplete-record-updates #-}

module Type.Crush.Unify
	( crushUnifyClass )
where

import Util
import qualified Debug.Trace	as Debug

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Shared.Error

import qualified Shared.Var	as Var
import Shared.Var		(NameSpace(..))

import Type.Exp
import Type.Error
import Type.State
import Type.Util
import Type.Class
import Type.Dump


-----
debug	= False
stage	= "Type.Crush.Unify"
trace s	= when debug $ traceM s


-----
-- unifyClass
--	Unify the elements in an equivalences classes queue.
--	If there are any errors then these are added to the SquidM monad.
--
crushUnifyClass 
	:: ClassId	
	-> SquidM Bool	-- whether this class was unified

crushUnifyClass	cidT
 = do	Just c	<- lookupClass cidT
	crushUnifyClass2 cidT c
	
crushUnifyClass2 cidT (ClassForward cid')
	= crushUnifyClass cid'

crushUnifyClass2 cidT c@Class{}

	-- class is already unified
	| []		<- classQueue c 
	, Just t	<- classType c
	= return False
	
	-- do some unification
	| otherwise
	= crushUnifyClass3 cidT c

crushUnifyClass2 cidT c
	= return False
	

crushUnifyClass3 cidT c
 = do	trace	$ "*   Unify.unifyClass " % cidT % "\n"
		% "    type         = " % classType c	% "\n"
		% "    name         = " % className c	% "\n"
		% "    queue        = " % classQueue c	% "\n"
		% "    nodes        = " % (map fst $ classNodes c) % "\n\n"

 	-- crush out nested unifiers and filter out vars and bottoms as they don't 
	--	contribute to the constructor
	let queue_clean	
		= filter 
			(\x -> case x of
				TVar{}	-> False
				TBot{}	-> False
				_	-> True)
			((maybeToList $ classType c) ++ classQueue c)

	trace	$ "    queue_clean  = " % queue_clean % "\n"

	-- If there is nothing left in the queue, or there's only one element
	--	then we're done. Otherwise call the reall unifier.
	t_final	<- case queue_clean of
			[] 	-> return		$ TBot (classKind c)
			[t] 	-> return		$ t
			_  	-> unifyClassMerge cidT c queue_clean
			
  	trace	$ "    t_final      = " % t_final	% "\n\n"

	-- Update the class with the new type
  	updateClass cidT c 
		{ classType 	= Just t_final
		, classQueue	= [] }

	-- Wake up any MPTCs acting on this class
	mapM activateClass $ Set.toList $ classFettersMulti c

	return True

-----
-- Merge the nodes in this class.
--
unifyClassMerge cidT c queue@(t:_)
	
	-- ClassIds
	| TClass k _	<- t
	= do
		let cids	=  map (\(TClass k cid) -> cid) queue
		cid		<- mergeClasses cids
		return	$ TClass k cid


	-- functions
 	| TFun{}	<- t
	, Just (t1s, t2s, effs, clos)
			<- liftM unzip4
			$ sequence 
			$ map (\x -> case x of
					TFun t1s t2s effs clos  -> Just (t1s, t2s, effs, clos)
					_			-> Nothing)
			$ queue
 	= do
		-- Merge args.
	 	[t1', t2']
			<- mapM mergeClassesT
			$  transpose
			$  zipWith (\t1 t2 -> [t1, t2])
				t1s 
				t2s

		-- Merge effects.
		let Just cidsE	= liftM catMaybes
				$ sequence 
				$ map (\e -> case e of
						TClass k cid	-> Just (Just cid)
						TBot k		-> Just Nothing
						_		-> Nothing)
				$ effs

		eff'		<- case cidsE of
					[]	-> return (TBot KEffect)
					_	-> return (TClass KEffect) `ap` mergeClasses cidsE
				

		-- Merge closures.
		let Just cidsC	= liftM catMaybes
				$ sequence
				$ map (\c -> case c of
						TClass k cid	-> Just (Just cid)
						TBot k		-> Just Nothing
						_		-> Nothing)
				$ clos		
		
		clo'		<- case cidsC of
					[]	-> return (TBot KClosure)
					_	-> return (TClass KClosure) `ap` mergeClasses cidsC
					
		return		$ TFun t1' t2' eff' clo'

	

	-- data		
	| TData v ts		<- t
	, Just vsTss		<- sequence 
					$ map (\x -> case x of
							TData v ts	-> Just (v, ts)
							_		-> Nothing)
					$ queue
				
	, (vs, tss)		<- unzip vsTss

	-- all ctors must have the same name.
	, length (nub vs) == 1

	-- also check number of args (kind) while we're here.
	, length (nub $ map length tss) == 1
	= do
		ts'	<- mapM mergeClassesT
			$  transpose 
			$  map (\(TData v ts) -> ts)
			$  queue

		return	$ TData v ts'

	-- Effect constructors.
	--	From the effect weakening rule it's always return a larger effect than needed.
	--	Therefore, if we want to "Unify" two effects E1 and E1 it's safe to return
	--	their l.u.b and use that inplace of both.
	| and $ map (\t -> kindOfType t == KEffect) queue
	= do	return	$ makeTSum KEffect queue

	-- .. likewise for closures
	| and $ map (\t -> kindOfType t == KClosure) queue
	= do	return	$ makeTSum KClosure queue


	-- Found a user type error in the graph
	| otherwise
 	= do	errorConflict cidT c 
		return (TError (kindOfType t) (classQueue c))


-----------------------
-- errorConflict
--	This function is called by unifyClassCheck when it finds a problem
--	in the graph. We diagnose the problem, add an error message to 
--	the SquidM monad and poison the class.
--
--	BUGS: 	only handles 2 conflicting ctors in the node.
--
errorConflict :: ClassId -> Class -> SquidM ()
errorConflict	 cid c
 = do	
	-- filter out TVars, as they don't conflict with anything
 	let tsCtorsNode
		= filter (\(t, _) -> not $ t =@= TVar{})
		$ classNodes c
 
	-- gather up the pairs that conflict
 	let conflicts
		= [ (n1, n2)
			| n1@(t1, _)	<- tsCtorsNode
			, n2@(t2, _)	<- tsCtorsNode
			, isConflict t1 t2 ]
	
	-- We could perhaps do some more extended diagnosis, but just
	--	report the first conflict for now.
	let Just ((t1, s1), (t2, s2))	
		= takeHead conflicts
	
	addErrors [
		ErrorUnifyCtorMismatch 
		{ eCtor1	= t1
		, eTypeSource1	= s1
		, eCtor2	= t2
		, eTypeSource2	= s2}]

	updateClass cid
		c { classType	= Just $ TError (classKind c) (classQueue c)}


	-- sanity, check that we've actually identified the problem and added
	--	an error to the state.
	errors	<- gets stateErrors
	when (isNil errors)
	 $ (panic stage
		 $ "errorConflict: Couldn't identify the error in class " % cid % "\n"
		 % "   type: \n" %> (classType c) % "\n\n"
		 % "   queue: \n" %> (classQueue c) % "\n\n"
		 % "   nodes:\n" %> ("\n" %!% classNodes c) % "\n\n")
	 
	return ()

-- Checks if these two types are conflicting 
isConflict :: Type -> Type -> Bool
isConflict t1 t2
	| TData v1 ts1	<- t1
	, TData v2 ts2	<- t2
	, v1 == v2
	, length ts1 == length ts2
	= False
	
	| TFun{}	<- t1
	, TFun{}	<- t2
	= False
	
	| TVar{}	<- t1	= False
	| TVar{}	<- t2	= False
	
	| TFetter{}	<- t1	= False
	| TFetter{}	<- t2	= False
	
	| otherwise
	= True



{-
	
unifyCheckTCon v argCount qq
 = case qq of
 	q@(TData v' ts') : qs
	 | v == v' && length ts' == argCount	-> unifyCheckTCon v argCount qs
	[]					-> False
	_					-> True

unifyCheckTFun qq
 = case qq of
 	q@(TFun{}) : qs				-> unifyCheckTFun qs
	[]					-> False
	_					-> True


-}
