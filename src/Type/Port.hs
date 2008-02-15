
module Type.Port
	( dropFMoresT
	, slurpContraClassVarsT 
	, portTypesT)
where

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Shared.Var	as Var

import Type.Exp
import Type.Class
import Type.Util
import Type.State
import Type.Plate.Collect
import Shared.Error
import Util

stage	= "Type.Port"

dropFMoresT :: Set Type -> Type -> SquidM Type
dropFMoresT tsContra tt
 = case tt of
 	TFetters fs t	
	 -> do	fs'	<- mapM (letifyFs tsContra) fs
	 	return	$ TFetters fs' t

	t -> return t
	
letifyFs tsContra ff
 = case ff of
 	FMore t1@(TClass k cid) t2
	 -> do	quantVars	<- gets stateQuantifiedVars
	 	v		<- makeClassName cid

		let result
			-- can't convert vars that have been quantified.
			| Map.member v quantVars 
			= ff
			
			-- can't convert vars that appear in contra-variant positions
			--	in the shape of the type
			| Set.member t1 tsContra
			= ff
			
			| otherwise
			= FLet t1 t2
			
		return result
			
	_ -> return ff




----
slurpContraClassVarsT :: Type -> [Type]
slurpContraClassVarsT tt
 = case tt of
	TForall vks t		-> slurpContraClassVarsT t
	TFetters fs t		-> slurpContraClassVarsT t
 	TFun t1 t2 eff clo	-> collectTClassVars t1 ++ slurpContraClassVarsT t2
	TData{}			-> []
	TVar{}			-> []
	TClass{}		-> []
	TError{}		-> []	
	_			-> panic stage
				$ "slurpContraClassVarsT: no match for " % tt

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

{-


-----
forcePortsT 
	:: Type	-> SquidM (Type, Table)
	
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

	return		$ (addFetters fs tPortR, useTable)



type	Table		
 = 	( [(Type, Type)]	-- contravatiant substitutions
 	, [(Type, Type)])	-- covariant substitutions

type	RenameM a 	
 = 	StateT Table SquidM a

renamePortsT ::	Type	-> SquidM (Type, Table)
renamePortsT t
 = do	(t', table')	<- runStateT (renamePortsCoT t) ([], [])
   	return (t', table')


-- Renaming variables in this type, where this type is taken to be 
--	in a co-variant position.
-- 
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
	
	FConstraint v ts
	 -> do	(subCon, subCo) <- get
	 	let sub		= subCon ++ subCo
		let ts'		= map (substituteTT (Map.fromList sub)) ts
		return	$ FConstraint v ts'

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
		

-- Renaming variables in this type, where this type is taken to be
--	in a contra-variant position.
--
renamePortsConT :: Type -> RenameM Type
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
	 | elem k [KEffect, KClosure]
	 -> renamePortCon t

	TClass k cid
	 | elem k [KEffect, KClosure]
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




-----
addToFetterTs :: [(Type, Type)] -> Type -> Type
addToFetterTs sub tt
 = case tt of
 	TFetters fs t	-> TFetters (map (addFetterTsF sub) fs) t
	_		-> tt
	
addFetterTsF sub ff
 	| FLet t1 t2	<- ff
	, Just t'	<- lookup t1 sub
	= FLet t1 (makeTSum (kindOfType t1) [t', t2])
	
	| otherwise
	= ff
	 
-}
