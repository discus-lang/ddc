
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
		% "    queue        = " % classQueue c	% "\n\n"


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

	-- all ctors must have the same name and the same number of args.
	, length (nub vs) == 1
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


	-- Oh oh..
	-- A conflict at this stage means that something has
	--	gone wrong internally and the graph is corrupted. 
	| otherwise
--	= return (TUnify (kindOfType t) queue)
	
 	= do	errorConflict cidT c 
		return (TError (kindOfType t) (classQueue c))

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
--	BUGS: 	only handles 2 conflicting ctors in the node.
--
errorConflict :: ClassId -> Class -> SquidM ()
errorConflict	 cid c
 = do	let (tsData, tsFun)	= splitDataFun (classNodes c) [] []
 	
	-- check for errors between data and function ctors
	when (not $ isNil tsFun)
	 $ do	errorConflictCF tsData tsFun
	 	let (tF1: tFs)	= tsFun
		errorConflictCC tF1 tFs
	 
	 
	-- check for errors between data ctors
	when (not $ isNil tsData)
	 $ do	let (tC1: tCs)	= tsData
	 	errorConflictCC tC1 tCs
		
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


splitDataFun [] ds fs 				= (ds, fs)
splitDataFun (x@(TData{}, _) : xs) ds fs	= splitDataFun xs (x : ds) fs
splitDataFun (x@(TFun{}, _)  : xs) ds fs	= splitDataFun xs ds (x : fs)
splitDataFun (_ : xs)		   ds fs	= splitDataFun xs ds fs
	
	
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
		
errorConflictCC x@(c@(TFun a1 b1 eff1 clo1), cInfo) tCs
 = case tCs of
 	[]				-> return ()

	(TFun a2 b2 eff2 clo2, _) : cs
	 -> errorConflictCC x cs
	 
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


