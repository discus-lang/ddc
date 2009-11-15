{-# OPTIONS -fno-warn-incomplete-record-updates #-}

module Type.Crush.Unify
	( crushUnifyClass 
	, isShallowConflict 
	, addErrorConflict )
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

import Type.Location
import Type.Exp
import Type.Error
import Type.State
import Type.Util
import Type.Class
import Type.Dump
import Type.Feed


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
--		% "    nodes        = " % (map fst $ classNodes c) % "\n\n"
		% "    nodes        = " % classNodes c % "\n\n"


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

unifyClassMerge cidT c queue
 = do	-- if one of the elements in the queue is a TApp then we'll need to break up
	-- all the other elements into TApp form to get them through the unifier.
	let hasTApp	= or $ map isTApp queue
	let queue_decon	= if hasTApp 
				then map deconstructT queue
				else queue
		
	trace $ "    queue_decon  = " % queue_decon % "\n"

	unifyClassMerge2 cidT c queue_decon
				

unifyClassMerge2 cidT c queue@(t:_)
	
	-- ClassIds
	| TClass k _	<- t
	= do
		let cids	=  map (\(TClass k cid) -> cid) queue
		cid		<- mergeClasses cids
		return	$ TClass k cid


	-- applications
	| TApp t1 t2	<- t
	, Just (t1s, t2s)
		<- liftM unzip $ sequence
		$ map (\x -> case x of
				TApp t1 t2 	-> Just (t1, t2)
				_		-> Nothing)
		$ queue
	= do
		let ?src	= TSNil "Type.Crush.Unify/TApp"
		Just t1s_fed	<- liftM sequence $ mapM (feedType Nothing) t1s
		Just t2s_fed 	<- liftM sequence $ mapM (feedType Nothing) t2s

		t1' <- mergeClassesT t1s_fed
		t2' <- mergeClassesT t2s_fed
		
		return	$ TApp t1' t2'

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
					[]	-> return tPure
					_	-> return (TClass kEffect) `ap` mergeClasses cidsE
				

		-- Merge closures.
		let Just cidsC	= liftM catMaybes
				$ sequence
				$ map (\c -> case c of
						TClass k cid	-> Just (Just cid)
						TBot k		-> Just Nothing
						_		-> Nothing)
				$ clos		
		
		clo'		<- case cidsC of
					[]	-> return tEmpty
					_	-> return (TClass kClosure) `ap` mergeClasses cidsC
					
		return		$ TFun t1' t2' eff' clo'

	-- data		
	| TData k v ts		<- t
	, Just vsTss		<- sequence 
					$ map (\x -> case x of
							TData k v ts	-> Just (v, ts)
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
			$  map (\(TData _ _ ts) -> ts)
			$  queue

		return	$ TData k v ts'

	-- Effect constructors.
	--	From the effect weakening rule it's always return a larger effect than needed.
	--	Therefore, if we want to "Unify" two effects E1 and E1 it's safe to return
	--	their l.u.b and use that inplace of both.
	| and $ map (\t -> kindOfType_orDie t == kEffect) queue
	= do	return	$ makeTSum kEffect queue

	-- .. likewise for closures
	| and $ map (\t -> kindOfType_orDie t == kClosure) queue
	= do	return	$ makeTSum kClosure queue


	-- Found a user type error in the graph
	| otherwise
 	= do	addErrorConflict cidT c 
		return (TError (kindOfType_orDie t) (classQueue c))

-----------------------
-- errorConflict
--	This function is called by unifyClassCheck when it finds a problem
--	in the graph. We diagnose the problem, add an error message to 
--	the SquidM monad and poison the class.
--
addErrorConflict :: ClassId -> Class -> SquidM ()
addErrorConflict  cid c
 = do	
	-- filter out TVars, as they don't conflict with anything
 	let tsCtorsNode
		= filter (\(t, _) -> not $ isSomeTVar t)
		$ classNodes c
 
	-- gather up the pairs that conflict
 	let conflicts
		= [ (n1, n2)
			| n1@(t1, _)	<- tsCtorsNode
			, n2@(t2, _)	<- tsCtorsNode
			, isShallowConflict t1 t2 ]
	
	-- We could perhaps do some more extended diagnosis, but just
	--	report the first conflict for now.
	case takeHead conflicts of
	 Just conf	-> addErrorConflict' cid c conf

	-- sanity, check that we've actually identified the problem and added
	--	an error to the state.
	 _ -> (panic stage
		 $ "errorConflict: Couldn't identify the error in class " % cid % "\n"
		 % "   type: \n" %> (classType c) % "\n\n"
		 % "   queue: \n" %> (classQueue c) % "\n\n"
		 % "   nodes:\n" %> ("\n" %!% classNodes c) % "\n\n")
	 
addErrorConflict' cid c ((t1, s1), (t2, s2))
 = do	
	addErrors [
		ErrorUnifyCtorMismatch 
		{ eCtor1	= t1
		, eTypeSource1	= s1
		, eCtor2	= t2
		, eTypeSource2	= s2}]

	updateClass cid
		c { classType	= Just $ TError (classKind c) (classQueue c)}

	return ()

-- Checks if these two types are conflicting 
isShallowConflict :: Type -> Type -> Bool
isShallowConflict t1 t2
	| TData _ v1 ts1	<- t1
	, TData _ v2 ts2	<- t2
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

-- Deconstruct a packed type into TApp form
deconstructT :: Type -> Type
deconstructT tt
 = case tt of
	TData k v ts
	 -> makeTApp (TCon (TyConData v k) : map deconstructT ts)
	
	TApp t1 t2
	 -> TApp (deconstructT t1) (deconstructT t2)
	
	TFun t1 t2 eff clo
	 -> TFun (deconstructT t1) (deconstructT t2) eff clo
	
	_ -> tt
	
	
	
	
	

