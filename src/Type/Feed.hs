{-# OPTIONS -fno-warn-incomplete-record-updates #-}

module Type.Feed
	( feedConstraint
	, feedType 
	, feedFetter
	, addFetter
	, addFetterSource)

where

import Util

import Shared.Error
import qualified Shared.Var as Var
import Shared.Var		(NameSpace (..))


import Type.Exp
import Type.Location
import Type.State
import Type.Class
import Type.Util
import Type.Link
import Type.Plate.FreeVars

import Constraint.Exp
import Constraint.Pretty

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set

import qualified Data.Array.IO	as Array

import qualified Debug.Trace

-----
stage	= "Type.Feed"

-- feedConstraint ----------------------------------------------------------------------------------
-- | Add a new constraint to the type graph.
--
feedConstraint 
	:: CTree -> SquidM ()
	
feedConstraint cc
 = case cc of
	-- Equality constraints. The LHS must be a variable.
 	CEq src (TVar k v1) t2
	 -> do
		-- create a new class for the LHS, with just that var in it.
	 	cid1		<- makeClassV src (kindOfSpace $ Var.nameSpace v1) v1

		-- feed the RHS into the graph.
		let ?src	= src
		Just (TClass _ cid2)
			<- feedType Nothing t2

		-- merge left and right hand sides.
		mergeClasses [cid1, cid2]
		return ()

	CEqs src ts
	 -> do	let ?src	= src
	 	Just ts'	<- liftM sequence
				$  mapM (feedType Nothing) ts
		let cids	= map (\(TClass k cid) -> cid) ts'
		mergeClasses cids
		return ()


	-- Signatures behave the same way as plain equality constraints.
	CSig src t1@(TVar k v1) t2
	 -> do 	feedConstraint (CEq src t1 t2)
		return ()

	-- Class constraints
	CClass src vC ts
	 -> do	let ?src	= src
		addFetter (FConstraint vC ts)
		return ()

	-- Projection constraints.
	CProject src j v1 tDict tBind
	 -> do	let ?src	= src
		addFetter (FProj j v1 tDict tBind)
		return ()		
				

	-- Type definitions, eg data constructors, external functions.
	CDef src (TVar k v1) t2
	 -> do	
	 	-- make a new class to hold the type.
		cid		<- makeClassV src (kindOfSpace $ Var.nameSpace v1) v1
	 
	 	-- add type to class
	 	addToClass cid src t2
		
		return ()


	_ -> 	panic stage
		 $ "feedConstraint: can't feed " % cc % "\n"

-- feedType --------------------------------------------------------------------------------------------
-- | Add a type to the type graph.
--	This always creates a new class and returns a classid.
--
feedType 	
	:: (?src :: TypeSource)
	-> Maybe ClassId 
	-> Type -> SquidM (Maybe Type)

feedType	mParent t
 = case t of
	TFetters fs t
	 -> do	
	 	-- Rename the vars on the LHS of FLet bindings to make sure
	 	--	they don't conflict with any vars already in the graph.
		vtSub		<- liftM (Map.fromList . catMaybes)
				$  mapM (\f -> case f of
					FLet (TVar k v1) t2	
					 -> do	v1'	<- newVarN (spaceOfKind k)
					 	return	$ Just (v1, TVar k v1')

					_ -> 	return	$ Nothing)
				$ fs

		let fs2		= substituteVT vtSub fs
		let t2		= substituteVT vtSub t
				
		mapM_ (feedFetter mParent) fs2
	 	t3		<- feedType mParent t2
		return	t3


	TSum k ts
	 -> do 	cidE		<- allocClass k
		Just es'	<- liftM sequence
				$  mapM (feedType1 (Just cidE)) ts
		addNode cidE 	$ TSum k es'

		returnJ		$ TClass k cidE

	TMask k t1 t2
	 -> do	cidE		<- allocClass k
		Just t1'	<- feedType1 (Just cidE) t1
		Just t2'	<- feedType1 (Just cidE) t2
		addNode cidE	$ TMask k t1' t2'
		
		returnJ		$ TClass k cidE

 	TVar k v 
	 -> do 	cidT		<- makeClassV ?src k v 
		returnJ		$ TClass k cidT

	TBot kind
	 -> do	cid		<- allocClass kind
		addNode cid	$ TBot kind
		returnJ		$ TClass kind cid

	TTop kind
	 -> do	cid		<- allocClass kind
		addNode cid	$ TTop kind
		returnJ		$ TClass kind cid


	-- data
	TFun t1 t2 eff clo
	 -> do	cidT		<- allocClass KData
		Just t1'	<- feedType (Just cidT) t1
		Just t2'	<- feedType (Just cidT) t2
		Just eff'	<- feedType (Just cidT) eff
		Just clo'	<- feedType (Just cidT) clo
		addNode cidT	$ TFun t1' t2' eff' clo'
		returnJ		$ TClass KData cidT

		
	TData v ts
	 -> do 	cidT		<- allocClass KData
		Just ts'	<- liftM sequence
				$  mapM (feedType (Just cidT)) ts

		addNode cidT 	$ TData v ts'
		returnJ		$ TClass KData cidT

	-- effect
	TEffect v ts
	 -> do 	cidE		<- allocClass KEffect
		Just ts'	<- liftM sequence
				$  mapM (feedType (Just cidE)) ts
		addNode cidE 	$ TEffect v ts'
		returnJ		$ TClass KEffect cidE

	-- closure
	-- TFree's that we get from the constraints might refer to types that are in
	--	the defs table but not the graph. We don't want to pollute the graph
	--	with the whole external def so trim these types down and just add the
	--	closure information that we need.
	TFree v1 t@(TVar KData v2)
	 -> do	cid		<- allocClass KClosure
		defs		<- gets stateDefs
		case Map.lookup v2 defs of
		 -- type that we're refering to is in the defs table
		 Just tDef
		  -> do	let tDef_trim	= trimClosureT Set.empty Set.empty $ flattenT tDef
		  	tDef'		<- linkType mParent [] tDef_trim

			-- we use addToClass here because we don't need to register effects and 
			--	classes for crushing in this type
			addToClass cid ?src $ TFree v1 tDef'
			returnJ		$ TClass KClosure cid

		 -- type must be in the graph
		 Nothing
		  -> do	t'		<- linkType mParent [] t
			addNode	cid	$ TFree v1 t'
			returnJ		$ TClass KClosure cid

	-- A non-var closure. We can get these from signatures and instantiated schemes
	TFree v1 t
	 -> do	cid		<- allocClass KClosure
		t'		<- linkType mParent [] t
		addNode	cid	$ TFree v1 t'
		returnJ		$ TClass KClosure cid


	TTag v
	 -> do	cid		<- allocClass KClosure
		addNode cid	$ TTag v
		returnJ		$ TClass KClosure cid

	TWild kind
	 -> do	cid		<- allocClass kind
		addNode cid	$ TWild kind
		returnJ		$ TClass kind cid

	TClass k cid
	 -> do 	cidT'		<- sinkClassId cid
		returnJ		$ TClass k cidT'
		
	_  ->	freakout stage
			( "feedType: cannot feed this type into the graph.\n"
			% "   type    = " % t 		% "\n"
			% "   source  = " % ?src 	% "\n"
			% "   mParent = " % mParent	% "\n")
			$ return Nothing

-----
-- feedType1
--	Feed a type into a type graph.
--	If the type is an Effect or Closure constructor then don't create a new class.
--	This is used to reduce the number of classes in the graph.
--
--	The constructors in sums from constraints such as: 
--		$e1 = ${!C a b, !D c d}
--	aren't points where unification can happen, so there is no reason to give them 
--	their own equivalence clases. 
--
--	This is unlike the effects from data or function constructors, ie.
--		t1  = a -(!e1 $c1)> b
--	here, t1 might be unified with another type, causing !e1 and $c1 to be unified 
--	also - so they need their own classes.
--

feedType1 
	:: (?src :: TypeSource)
	-> Maybe ClassId
	-> Type -> SquidM (Maybe Type)
		
feedType1 mParent tt
 = case tt of
	TTop k
	 | elem k [KEffect, KClosure]	
	 -> returnJ tt
	 
	TBot k
	 | elem k [KEffect, KClosure]
	 -> returnJ tt

	TSum k []
	 ->	returnJ tt
 
 	-- effects
 	TEffect v ts
	 -> do	Just ts' <- liftM sequence
	 		 $  mapM (feedType mParent) ts
	 	returnJ	$ TEffect v ts'
		
	-- closures
	TFree v t
	 -> do	Just tt'	<- feedType mParent tt
	 	returnJ	$ tt'

	TTag{}
	 ->	returnJ	$ tt

	_ 	-> feedType mParent tt


-- Fetter ------------------------------------------------------------------------------------------
feedFetter 
	:: (?src :: TypeSource)
	-> Maybe ClassId
	-> Fetter 
	-> SquidM ()

feedFetter	mParent f
 = case f of
	FLet t1 t2
	 -> do	Just (TClass k1 cid1)	<- feedType mParent t1
	 	Just (TClass k2 cid2)	<- feedType mParent t2
		mergeClasses [cid1, cid2]
		return ()

	FMore t1 t2	
	 -> 	feedFetter mParent (FLet t1 t2)

	FConstraint v ts
	 -> do	addFetter f
	 	return ()

	FProj pj v tDict tBind
	 -> do	cidC		<- allocClass KFetter
	 	Just [tDict', tBind']	
				<- liftM sequence
				$  mapM (feedType (Just cidC)) [tDict, tBind]

		addNode cidC	$ TFetter (FProj pj v tDict' tBind')
		


-----
addNode :: (?src :: TypeSource)
	-> ClassId
	-> Type
	-> SquidM ()
	
addNode    cidT	t
 = do	addToClass cidT	?src t
	activateClass cidT



-- addFetter ---------------------------------------------------------------------------------------
-- | Add a new fetter constraint to the graph
addFetterSource :: TypeSource -> Fetter -> SquidM Bool
addFetterSource src f 
	= let 	?src	= src
	  in	addFetter f


addFetter
	:: (?src :: TypeSource)
	-> Fetter
	-> SquidM Bool
	
-- Single parameter type class contraints are added directly to the equivalence
--	class which they constrain.
--
addFetter (FConstraint vC [t1])
 = do	
 	-- work out the equivalence class that this constraint refers to
 	cid1	<- case t1 of
 			TVar k v1	-> makeClassV ?src (kindOfSpace $ Var.nameSpace v1) v1
			TClass k cid	-> return cid

	let k	= case t1 of
			TVar k1 _	-> k1
			TClass k1 _	-> k1

	-- add the fetter to the equivalence class
	let f	= FConstraint vC [TClass k cid1]

	Just c		<- lookupClass cid1
	let fsThere	= map (\(TFetter f) -> f) $ classFetters c

	-- add the fetter even if it's already there so we get the extra classNode
 	modifyClass cid1
	 $ \c -> c	
	 	{ classFetters	= nub $ (TFetter f) 		: classFetters c
		, classNodes	= nub $ (TFetter f, ?src) 	: classNodes c }

	if (elem f $ fsThere)
	 then	return False
	 else do
	 	activateClass cid1
		return True
			
	
-- Multi parameter type class constraints are added as ClassFetter nodes in the graph 
--	and the equivalence classes which they constraint hold ClassIds which point to them.
--
addFetter f@(FConstraint v ts)
 = do 	
 	-- create a new class to hold this node
	cidF		<- allocClass KFetter
	 	
	-- add the type args to the graph
	Just ts'	<- liftM sequence
			$  mapM (feedType (Just cidF)) ts

	-- add the fetter to the graph
	let f	= FConstraint v ts'

	modifyClass cidF
	 $ \c -> ClassFetter
	 	{ classId	= cidF
		, classFetter	= f 
		, classSource	= ?src }

	activateClass cidF

	-- work out what cids this constraint is acting on
	let cids	= map (\(TClass k cid) -> cid) ts'
	
	-- add a reference to this constraint to all those classes
	mapM	(\cid -> modifyClass cid
			$ \c -> c { classFettersMulti = Set.insert cidF (classFettersMulti c) })
		$ cids

	return True

addFetter f@(FProj j v1 tDict tBind)
 = do
 	-- a new class to hold this node
 	cidF	<- allocClass KFetter
	
	-- add the type args to the graph
 	Just [tDict', tBind']
		<- liftM sequence
		$  mapM (feedType (Just cidF)) [tDict, tBind]

	-- add the fetter to the graph
	let f	= FProj j v1 tDict' tBind'
	
	modifyClass cidF
	 $ \c -> ClassFetter
	 	{ classId	= cidF
		, classFetter	= f 
		, classSource	= ?src }
		
	activateClass cidF
	
	-- add a reference to the constraint to all those classes it is acting on
	let cids	= map (\(TClass k cid) -> cid) [tDict', tBind']

	mapM	(\cid -> modifyClass cid
			$ \c -> c { classFettersMulti = Set.insert cidF (classFettersMulti c) })
		cids
			
	return True
	

