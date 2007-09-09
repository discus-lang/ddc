
module Type.Port
	( portTypesT
	, forcePortsT
	, renamePortsT)
where

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Shared.Var	as Var

import Util
import Type.Exp
import Type.Util
import Type.State

-----
-- portTypesT
--	Port cids are the effect and closure cids via which useful information
--	is passed into the type scheme.
--
--	These are the cids which appear contra-variantly in the type, ie to the
--	left of a function arrow.
--
portTypesT ::	Type -> [Type]
portTypesT    t
 = case t of
	TFetters fs t		-> portTypesT t
 	TFun	t1 t2 eff clo	-> portTypesC t1 ++ portTypesT t2
	TData	v ts		-> catMap portTypesT ts
	_			-> []
	 
portTypesC t
 = case t of
 	TFun	t1 t2 eff clo	
	 -> 	[ eff, clo ]
	 ++ portTypesC t1 
	 ++ portTypesC t2

	TData	v ts		-> catMap portTypesC ts

	TVar KEffect v		-> [t]
	TVar KClosure v		-> [t]

	_			-> []




-----
forcePortsT 
	:: Type	-> SquidM Type
	
forcePortsT t
 = do
	(tPortR, useTable)	
			<- renamePortsT t
	
	let (subCon, subCo)	
			= useTable
	
	let subInput	= gather subCon
	let output	= [(output, node : fromMaybe [] (lookup node subInput))	
				| (node, output)	<- subCo]
	
	let fs		= [FLet t (makeTSum (kindOfType t) ts)
				| (t, ts)		<- output]

	return		$ addFetters fs tPortR



type	Table		
 = 	( [(Type, Type)]
 	, [(Type, Type)])

type	RenameM a 	
 = 	StateT Table SquidM a

renamePortsT ::	Type	-> SquidM (Type, Table)
renamePortsT t
 = do	(t', table')	<- runStateT (renamePortsCoT t) ([], [])
   	return (t', table')

-----
renamePortsCoT
	:: Type -> RenameM Type
	
renamePortsCoT t
 = case t of
 	TFetters fs x
	 -> do	x'	<- renamePortsCoT x
		fs'	<- mapM (renamePortsCoF) fs
	 	return	$ TFetters fs' x'

	TFun t1 t2 eff clo
	 -> do	t1'	<- renamePortsConT t1

	 	[t2', eff', clo'] 	
			<- mapM renamePortsCoT [t2, eff, clo]
			
		return	$ TFun t1' t2' eff' clo'
		
	TData v ts
	 -> do	ts'	<- mapM renamePortsCoT ts
	 	return	$ TData v ts'
		
	TVar k v
	 | elem k [KEffect, KClosure, KRegion]
	 -> renamePortCo t

	TClass k cid
	 | elem k [KEffect, KClosure, KRegion]
	 -> renamePortCo t
	 
	_	-> return t


renamePortsCoF 
	:: Fetter -> RenameM Fetter
	
renamePortsCoF f
 = case f of
 	FLet t1 t2	
	 -> do	(subCon, subCo)	<- get
	 	let sub		= subCon ++ subCo
	 	let t2'		= substituteTT (Map.fromList sub) t2
	 	return	$ FLet t1 t2'
	

renamePortCo :: Type -> RenameM Type
renamePortCo t
 = do	(subCon, subCo)	<- get
 
 	case lookup t subCo of
	 Just t'	-> return t'
	 Nothing
	  -> do	let k	= kindOfType t
	  	v	<- lift (newVarN $ spaceOfKind k)
	  	let t'	= TVar k v
		put	(subCon, (t, t') : subCo)
		return	t'
		
-----
renamePortsConT t
 = case t of
 	TFun t1 t2 eff clo
	 -> do	[t1', t2', eff', clo']
	 		<- mapM renamePortsConT [t1, t2, eff, clo]
			
		return	$ TFun t1' t2' eff' clo'
		
	TData v ts
	 -> do	ts'	<- mapM renamePortsConT ts
	 	return	$ TData v ts'

	TVar k v
	 | elem k [KEffect, KClosure, KRegion]
	 -> renamePortCon t

	TClass k cid
	 | elem k [KEffect, KClosure, KRegion]
	 -> renamePortCon t
	 
	_	-> return t


renamePortCon 
	:: Type	-> RenameM Type
	
renamePortCon t
 = do	(subCon, subCo)	<- get
 
 	let k	= kindOfType t
	v	<- lift (newVarN $ spaceOfKind k)
	let t'	= TVar k v
	put	((t, t') : subCon, subCo)
	return	t'


{-		
renamePortsT :: Type 
	-> SquidM 
		( Type
		, [Use])

renamePortsT t
 = case t of
 	TFetters fs x	
	 -> do	(x', sub)	<- renamePortsT x
		
--		let fsAdd	= foldr (\(e, p) x -> addSumFs e p x) fs sub
--	 	return		( TFetters fsAdd t', sub)
		
		return		( TFetters fs x', sub)


	TFun t1 t2 eff clo
	 -> do	(t1', sub1)	<- renamePortsC t1

	 	([t2', eff', clo'], subs2)	
			<- liftM unzip $ mapM renamePortsT [t2, eff, clo]

		return		( TFun t1' t2' eff' clo'
				, sub1 ++ concat subs2)
		
	TData v ts
	 -> do	(ts', subs)	
	 		<- liftM unzip $ mapM renamePortsT ts

	 	return		( TData v ts'
				, concat subs)

	TVar k v
	 | elem k [KEffect, KClosure, KRegion]
	 -> do	t'	<- renamePort t
	 	return	(t', [Co (t, t')])

	TClass k cid
	 | elem k [KEffect, KClosure, KRegion]
	 -> do	t'	<- renamePort t
	 	return	(t', [Co (t, t')])
		
	_ -> 	return 		(t, [])
	
	
renamePortsC :: Type -> SquidM (Type, [Use])
renamePortsC t
 = case t of
 	TFun t1 t2 eff clo
	 -> do	([t1', t2', eff', clo'], subss)
	 		<- liftM unzip $ mapM renamePortsC [t1, t2, eff, clo]
		
		return	( TFun t1' t2' eff' clo'
			, concat subss)

	TData v ts
	 -> do	(ts', subs)	
	 		<- liftM unzip $ mapM renamePortsC ts

	 	return	( TData v ts', concat subs)
		
	TVar k v
	 | elem k [KEffect, KClosure, KRegion]
	 -> do	t'	<- renamePort t
	 	return	(t', [Con (t, t')])

	TClass k cid
	 | elem k [KEffect, KClosure, KRegion]
	 -> do	t'	<- renamePort t
	 	return	(t', [Con (t, t')])
		
	_ ->	return	(t, [])


renamePort :: Type -> SquidM Type
renamePort t@(TVar k v)
 = do	v'	<- newVarN $ Var.nameSpace v
 	return	$ TVar k v'
	
renamePort t@(TClass k v)
 = do	v'	<- newVarN $ spaceOfKind k
 	return	$ TVar k v'

	 



addSumFs :: Type -> Type -> [Fetter] -> [Fetter]
addSumFs t1 tAdd []	
	= [FLet t1 tAdd]

addSumFs t1 tAdd (f@(FLet t2 tRHS) : fs)
	| t1 == t2	
	= FLet t1 (makeSumT (kindOfType t1) [tRHS, tAdd]) 
	: fs

addSumFs t1 tAdd (f:fs)
	= f		: addSumFs t1 tAdd fs



-- not used

traceContraNodesT :: [(Type, Type)] -> Type -> Set Type
traceContraNodesT binds tt
 = case tt of
	TFetters fs tt
	 -> let	binds'	= binds ++ [(t1, t2) | FLet t1 t2 <- fs]
	    in	traceContraNodesT binds' tt

 	TFun t1 t2 eff clo
	 -> let	ts1	= traceContraNodesC binds t1
	 	ts2	= traceContraNodesT binds t2
	    in	ts1 `Set.union` ts2
	    
	TData v ts
	 -> 	Set.unions $ map (traceContraNodesT binds) ts
	    
	TVar{}		-> fromMaybe Set.empty $ liftM (traceContraNodesT binds) $ lookup tt binds
	TClass{}	-> fromMaybe Set.empty $ liftM (traceContraNodesT binds) $ lookup tt binds
		

traceContraNodesC :: [(Type, Type)] -> Type -> Set Type
traceContraNodesC binds tt
 = case tt of
 	TFetters fs tt
	 -> let	binds'	= binds ++ [(t1, t2) | FLet t1 t2 <- fs]
	    in	traceContraNodesC binds' tt
	 
	TFun t1 t2 eff clo
	 -> 	Set.unions $ map (traceContraNodesC binds) [t1, t2, eff, clo]
	    
	TData v ts
	 -> 	Set.unions $ map (traceContraNodesC binds) ts
	 
	TVar k _
	 -> case k of
	 	KData 	
		 -> Set.insert tt 
			$ fromMaybe Set.empty 
			$ liftM (traceContraNodesC binds) 
			$ lookup tt binds

		_	-> Set.singleton tt
		
		
	TClass k _
	 -> case k of
	 	KData	
		 -> Set.insert tt
		 	$ fromMaybe Set.empty 
			$ liftM (traceContraNodesC binds) 
			$ lookup tt binds

		_	-> Set.singleton tt


-}
