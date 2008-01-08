
module Type.Feed
	( feedConstraint
	, feedType 
	, feedFetter
	, registerNodeT
	, registerNodeF )

where

import Util

import Shared.Error
import qualified Shared.Var as Var
import Shared.Var		(NameSpace (..))

import qualified Data.Map	as Map
import Data.Map			(Map)

import Type.Exp
import Type.State
import Type.Class
import Type.Util
import Type.Link
import Type.Plate.FreeVars

import Constraint.Exp
import Constraint.Pretty

import qualified Data.Array.IO	as Array

-----
stage	= "Type.Feed"

-----------------------
-- feedConstraint
--	Add a new constraint to the type graph.
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
	 -> do
	 	feedConstraint (CEq src t1 t2)
		return ()


	-- Class constraints.
	CClass src v ts
	 -> do	let ?src	= src

	 	-- a new class to hold this node
	 	cid		<- allocClass KFetter
	 	
		-- add the type args to the graph
	 	Just ts'	<- liftM sequence
				$  mapM (feedType (Just cid)) ts
		
		-- add the constraint
		graph	<- gets stateGraph
		let c	= ClassFetter
			{ classId	= cid
			, classFetter	= FConstraint v ts' }
		
		liftIO (Array.writeArray (graphClass graph) cid c)
		registerClass (Var.bind v) cid
		return ()		


	-- Projection constraints.
	CProject src j v1 t2 t3 eff clo
	 -> do	let ?src	= src

	 	-- a new class to hold this node
	 	cid		<- allocClass KFetter
	 	
		-- add the type args to the graph
	 	Just [t2', t3', eff', clo'] 
				<- liftM sequence
				$  mapM (feedType (Just cid)) [t2, t3, eff, clo]
		
		-- add the constraint
		graph	<- gets stateGraph
		let c	= ClassFetter
			{ classId	= cid
			, classFetter	= FProj j v1 t2' t3' eff' clo' }
		
		liftIO (Array.writeArray (graphClass graph) cid c)
		registerClass Var.FProj cid
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

-----------------------
-- feedType
--	Add a type to the type graph.
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
		patchBackRef cidE mParent

		Just es'	<- liftM sequence
				$  mapM (feedType1 (Just cidE)) ts
		addNode cidE 	$ TSum k es'

		returnJ		$ TClass k cidE

	TMask k t1 t2
	 -> do	cidE		<- allocClass k
	 	patchBackRef cidE mParent
		
		Just t1'	<- feedType1 (Just cidE) t1
		Just t2'	<- feedType1 (Just cidE) t2
		addNode cidE	$ TMask k t1' t2'
		
		returnJ		$ TClass k cidE

 	TVar k v 
	 -> do 	cidT		<- makeClassV ?src k v 
		patchBackRef cidT mParent
		returnJ		$ TClass k cidT

	TBot kind
	 -> do	cid		<- allocClass kind
	 	patchBackRef cid mParent
		addNode cid	$ TBot kind
		returnJ		$ TClass kind cid

	TTop kind
	 -> do	cid		<- allocClass kind
	 	patchBackRef cid mParent
		addNode cid	$ TTop kind
		returnJ		$ TClass kind cid


	-- data
	TFun t1 t2 eff clo
	 -> do	cidT		<- allocClass KData
		patchBackRef cidT mParent 

		Just t1'	<- feedType (Just cidT) t1
		Just t2'	<- feedType (Just cidT) t2
		Just eff'	<- feedType (Just cidT) eff
		Just clo'	<- feedType (Just cidT) clo
		addNode cidT	$ TFun t1' t2' eff' clo'
		returnJ		$ TClass KData cidT

		
	TData v ts
	 -> do 	cidT		<- allocClass KData
		patchBackRef cidT mParent

		Just ts'	<- liftM sequence
				$  mapM (feedType (Just cidT)) ts

		addNode cidT 	$ TData v ts'
		returnJ		$ TClass KData cidT

	-- effect
	TEffect v ts
	 -> do 	cidE		<- allocClass KEffect
		patchBackRef cidE mParent
	
		Just ts'	<- liftM sequence
				$  mapM (feedType (Just cidE)) ts
		addNode cidE 	$ TEffect v ts'
		returnJ		$ TClass KEffect cidE

	-- closure
	TFree v t
	 -> do	cid		<- allocClass KClosure
	 	patchBackRef cid mParent
		
		t'		<- linkType mParent [] t
		addNode	cid	$ TFree v t'
		returnJ		$ TClass KClosure cid

	TTag v
	 -> do	cid		<- allocClass KClosure
	 	patchBackRef cid mParent
		
		addNode cid	$ TTag v
		returnJ		$ TClass KClosure cid

	TWild kind
	 -> do	cid		<- allocClass kind
	 	patchBackRef cid mParent
		addNode cid	$ TWild kind
		returnJ		$ TClass kind cid

	TClass k cid
	 -> do 	cidT'		<- sinkClassId cid
		patchBackRef cid mParent
		returnJ		$ TClass k cidT'
		
	TAccept t
	 -> do	let k		= kindOfType t
	 	cid		<- allocClass k
		patchBackRef cid mParent

	 	Just t'		<- feedType mParent t
		
		addNode cid 	$ TAccept t'
	 	returnJ		$ TClass k cid

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
	 -> do	Just t'	<- feedType mParent t
	 	returnJ	$ TFree v t'

	TTag{}
	 ->	returnJ	$ tt

	_ 	-> feedType mParent tt


-----------------------
-- feedFetter
--
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
	 -> feedFetter mParent (FLet t1 t2)

	FConstraint v ts
	 -> do 	addFetterNode f
		return ()

	FProj pj t1 t2 t3 eff clo
	 -> do	cidC		<- allocClass KFetter
	 	Just [t2', t3']	<- liftM sequence
				$  mapM (feedType (Just cidC)) [t2, t3]
		Just eff'	<- feedType (Just cidC) eff
		Just clo'	<- feedType (Just cidC) clo

		addNode cidC	$ TFetter (FProj pj t1 t2' t3' eff' clo')
		
	_ -> panic stage 
		$ pretty
		$ "feedFetter: cannot feed " % f % "\n"
		% "      " % show f % "\n"


-----
addNode :: (?src :: TypeSource)
	-> ClassId
	-> Type
	-> SquidM ()
	
addNode    cidT	t
 = do	registerNodeT cidT t
 	addToClass cidT	?src t


-- | Add a new fetter constraint to the graph
addFetterNode 
	:: (?src :: TypeSource)
	-> Fetter 
	-> SquidM ClassId

addFetterNode f@(FConstraint v ts)
 = do 	-- a new class to hold this node
	cid		<- allocClass KFetter
	 	
	-- add the type args to the graph
	Just ts'	<- liftM sequence
			$  mapM (feedType (Just cid)) ts
		
	-- add the constraint
	graph	<- gets stateGraph
	let c	= ClassFetter
		{ classId	= cid
		, classFetter	= FConstraint v ts' }
	
	liftIO (Array.writeArray (graphClass graph) cid c)
	registerNodeF cid f
	
	return cid


-- | Register a type node.
registerNodeT :: ClassId -> Type -> SquidM ()
registerNodeT	cid tt
	| elem Var.EReadH
		$ map Var.bind 
		$ freeVarsT tt
	= registerClass Var.EReadH cid

	| elem Var.EReadT
		$ map Var.bind 
		$ freeVarsT tt
	= registerClass Var.EReadT cid

	| otherwise
	= return ()				

-- | Register a fetter node.
registerNodeF :: ClassId -> Fetter -> SquidM ()
registerNodeF cid ff
 = case ff of
 	FConstraint v ts
 	 -- Register shape constraints with arity to zero so that they're all recorded
	 --	in the same register slot.
	 | isFShape (Var.bind v)
	 -> registerClass (Var.FShape 0) cid

	 | elem (Var.bind v) [Var.FLazyH, Var.FMutableT, Var.FConstT]
	 -> registerClass (Var.bind v) cid
	 
	_ 
	 -> return ()


isFShape (Var.FShape _)	= True
isFShape _		= False
	

{-
 = case t of
	-- effects
 	TSum KEffect [TEffect v _]			-- ReadH
	 | Var.bind v == Var.EReadH 
	 -> registerClass Var.EReadH cid

	 | Var.bind v == Var.EReadT			-- ReadT
	 -> registerClass Var.EReadT cid

	 | Var.bind v == Var.EWriteT			-- WriteT
	 -> registerClass Var.EWriteT cid

	-- fetters
 	TFetter (FConstraint v _)
	 | Var.bind v == Var.FLazyH			-- LazyH
	 -> registerClass Var.FLazyH cid

	 | Var.bind v == Var.FMutableT			-- MutableT
	 -> registerClass Var.FMutableT cid
	 
	 | Var.bind v == Var.FConstT			-- ConstT
	 -> registerClass Var.FConstT cid


	TFetter (FProj{})				-- Proj
	 -> registerClass Var.FProj cid
-}
