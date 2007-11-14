

module Type.Crush.Sum
	( crushSumClass )
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
debug	= True
stage	= "Type.Crush.Sum"
trace s	= when debug $ traceM s



crushSumClass :: ClassId -> SquidM ()
crushSumClass cid
 = do 	Just c	<- lookupClass cid
	let t	= classType c
	
	case t of
		TSum KData ts	-> crushSum cid c t
		_		-> return ()
		
crushSum :: ClassId -> Class -> Type -> SquidM ()
crushSum cid c t@(TSum k ts) 
 = do	
	trace	$ "*   Sum.crushSum " % cid % "\n"
		% "    t        = " % t 	% "\n"

 	let ts'		= flattenTSum t
 	tsClass		<- mapM loadType ts'

	trace	$ "    tsClass  = " % tsClass	% "\n"

	t'		<- mergeSum cid c t tsClass
	
	trace	$ "    t'       = " % t'	% "\n"
		% "\n"
	
	updateClass cid c { classType = t' }
	

loadType :: Type -> SquidM Type
loadType tt
 = case tt of
 	TClass k cid
	 -> do	Just c	<- lookupClass cid
	 	return	$ classType c
		
	_ ->	return tt



mergeSum :: ClassId -> Class -> Type -> [Type] -> SquidM Type
mergeSum cid c t ts

	| Just (t1s, t2s, effs, clos)	
		<- liftM unzip4 
		$  sequence 
		$  map snocTFun ts
	= do
		t1	<- sumContraTS t1s
		t2	<- sumContraTS t2s
		eff	<- sumCovarTS effs
		clo	<- sumCovarTS clos
		
		let t	= TFun 	t1	t2 eff clo
		return t


	| Just (vs, tss)		
		<- liftM unzip
		$  sequence 
		$  map snocTData ts

	, (v1:vsRest)	<- vs
	, and $ map (== v1) vsRest
	= do
		ts'	<- mapM sumCovarTS $ transpose tss
		let (v1: _)	= vs
		
		let t	= TData v1 ts'
		
		return	t					 	

	| otherwise
	= return $ t


sumCovarTS :: [Type] -> SquidM Type
sumCovarTS ts@(t1 : tRest)
 = case kindOfType t1 of
--  	KData
--	 -> do	mergeClassesT TUnify ts
	 
	_ 
	 -> do	let k	= kindOfType t1
		cid	<- allocClass (kindOfType t1)
		addToClass cid TSNil (makeTSum k ts)
		return	$ TClass k cid


sumContraTS :: [Type] -> SquidM Type
sumContraTS ts@(t1 : tRest)
	= mergeClassesT TUnify ts 	


	
		
	
 	


