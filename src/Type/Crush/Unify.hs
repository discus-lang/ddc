
module Type.Crush.Unify
	( crushUnifyClass )
where

import Util
import qualified Debug.Trace	as Debug

import qualified Data.Map	as Map
import Data.Map			(Map)

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
crushUnifyClass :: ClassId	-> SquidM ()
crushUnifyClass	cidT
 = do
	Just c	<- lookupClass cidT
	let t	= classType c

	trace	$ "*   Unify.unifyClass " % cidT % "\n"
		% "    t       = " % t % "\n"

	-- crush out nested unifiers and filter out vars.
	let ts	= flattenTUnify t
	let ts2	= filter (\x -> not $ x =@= TVar{}) ts

	trace	$ "    ts2     = " % ts2 % "\n"

	t'	<- case ts2 of
			[] 	-> return	$ TBot (classKind c)
			[t] 	-> return	$ t
			_  	-> unifyClassMerge cidT c ts2
			
  	trace	$ "    t'      = " % t'	% "\n\n"
  	updateClass cidT c { classType = t' }
	




-----
-- Merge the nodes in this class.
--
unifyClassMerge cidT c queue@(t:_)
	
	-- ClassIds
	| TClass k _	<- t
	= do
		let cids	=  map (\(TClass k cid) -> cid) queue
		cid		<- mergeClasses makeTUnify cids
		
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
			<- mapM (mergeClassesT makeTUnify)
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
					_	-> return (TClass KEffect) `ap` mergeClasses makeTUnify cidsE
				

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
					_	-> return (TClass KClosure) `ap` mergeClasses makeTUnify cidsC
					
		return		$ TFun t1' t2' eff' clo'

	

	-- data		
	| TData v ts		<- t
	, Just vsTss		<- sequence 
					$ map (\x -> case x of
							TData v ts	-> Just (v, ts)
							_		-> Nothing)
					$ queue
				
	, (vs, tss)		<- unzip vsTss

	, length (nub vs) == 1
	, length (nub $ map length tss) == 1
	= do
		ts'	<- mapM (mergeClassesT makeTUnify)
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


	-- Oh oh..
	-- A conflict at this stage means that something has
	--	gone wrong internally and the graph is corrupted. 
	| otherwise
--	= return (TUnify (kindOfType t) queue)
	
 	= do	errorConflict cidT c 
		return (TError (kindOfType t) (classType c))

{-	panic stage
	$ "unifyClass: Found unexpected conflict in type graph.\n"
	% "    cid = " % cidT 	% "\n"
	% "  queue = " % queue 	% "\n"

	% "nodes:\n"
	% catMap (\(t, ts) -> pretty $ t % " " % ts % "\n") (classNodes c)
-}

-----------------------
-- errorConflict
--	This function is called by unifyClassCheck when it finds a problem
--	in the graph. We diagnose the problem, add an error message to 
--	the SquidM monad and poison the class.
--
errorConflict :: ClassId -> Class -> SquidM ()
errorConflict	 cid c
 = do
 	let [tCons@(tC1: tCs), tFuns]
		= partitionFs
			[ (\(t, ti) -> t =@= TData{})
			, (\(t, ti) -> t =@= TFun{}) ]
		$ classNodes c
 	
	errorConflictCF tFuns tCons
	errorConflictCC tC1 tCs
	
	updateClass cid
		c { classType	= TError (classKind c) (classType c)}

	
errorConflictCF tCons tFuns
 = case (tCons, tFuns) of
 	([], [])	-> return ()
	([],  _)	-> return ()
	(_,  [])	-> return ()

	(  ((c, cInfo) : _)
	 , ((f, fInfo) : _))
	 -> addErrors [
		ErrorUnifyCtorMismatch 
		{ eCtor1	= f
		, eTypeSource1	= fInfo
		, eCtor2	= c
		, eTypeSource2	= cInfo}]


errorConflictCC x@(c@(TData v ts), cInfo) tCs
 = case tCs of
 	[]				-> return ()
	(TData v' ts', _) : cs
	 | v         == v'
	 , length ts == length ts'	-> errorConflictCC x cs
	 
	(c', cInfo') : cs
	 -> addErrors [
		ErrorUnifyCtorMismatch 
		{ eCtor1	= c
		, eTypeSource1	= cInfo
		, eCtor2	= c'
		, eTypeSource2	= cInfo'}]
		


-----
-- Check for type conflicts in this class.
--	These are the conflicts which are likely to arise from 
--	bugs in the user code.
--			
unifyClassCheck cidT c queue@(q:qs)
 = case q of
{- 	TData v ts
	 |  unifyCheckTCon v (length ts) qs 	
	 -> errorConflict  cidT c 
	 
	TFun{}
	 |  unifyCheckTFun qs			
	 -> errorConflict  cidT c
-}	 
	_ -> unifyClassMerge cidT c queue
	
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


