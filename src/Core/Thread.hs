
-- | Thread witness variables through the program to replace statically constructed
--	witnesses in type applications.
--
--   Static witness applications are things like:
--	f %r1 (Const %r1)
--
--   	In this application (Const %r1) satisfies the constraint, but it's not
--	syntactically sound. We might also use (Mutable %r1) somewhere else in the
--	program.
--	
module Core.Thread 
	(threadTree)
	
where

import Core.Plate.Walk
import Core.Util
import Core.ReconKind
import Core.Exp

import qualified Shared.Var	as Var
import qualified Shared.VarBind	as Var
import Shared.VarPrim
import Shared.Error
import Util

import Control.Monad.State

import qualified Util.Map	as Map
import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Debug.Trace	as Debug


-----
stage	= "Core.Thread"


--------------------------------------------------------------------------------
-- Maintain a map of regions, to what witnesses we have available for them.
type ThreadS
	= [((Var, Var), Var)]	-- (class name, region), witness variable
	

type ThreadM 
	= State ThreadS

-- A map of the defined type class instances
type ClassInstMap
	= Map Var [Top]

--------------------------------------------------------------------------------

-- | Thread witness variables in this tree
threadTree :: Tree -> Tree -> Tree
threadTree hTree sTree 
 = let	instMap	= slurpClassInstMap (sTree ++ hTree)
   
	walkTable 
		 = walkTableId
		 { transP	= thread_transP 
		 , transX 	= thread_transX instMap 
		 , transX_enter	= thread_transX_enter }
	
   in	evalState (walkZM walkTable sTree) []

-- | Slurp out all the class instances from ths tree
slurpClassInstMap
 ::	Tree	-> Map Var [Top]

slurpClassInstMap tree
 = 	Map.gather
 	[ (v, p)	| p@(PClassInst v ts context defs) <- tree]


-- | push witnesses to properties of top-level regions.
thread_transP :: WalkTable ThreadM -> Top -> ThreadM Top
thread_transP table pp
 = case pp of
 	PRegion r vts
	 -> do	mapM_ (\(v, t) -> pushWitnessVK v (kindOfType t)) vts
	 	return pp

	_ -> return pp


-- | bottom-up: replace applications of static witnesses with bound witness variables.
thread_transX :: ClassInstMap -> WalkTable ThreadM -> Exp -> ThreadM Exp
thread_transX instMap table xx
 = case xx of
 	XAPP x t@(TClass{})
	 -> do	t'	<- rewriteWitness instMap t
	 	return	$ XAPP x t'

	-- pop lambda bound witnesses on the way back up because we're leaving their scope.
	XLAM b k x
	 -> do	popWitnessVK (varOfBind b) k
	 	return	xx

	-- pop locally bound witnesses on the way back up because we're leaving their scope.
	XLocal r vts x
	 -> do	mapM_ (\(v, k) -> popWitnessVK v k)
	 		[ (v, kindOfType t)	| (v, t) <- reverse vts]
		
	 	return xx

	_ ->	return xx


thread_transX_enter :: WalkTable ThreadM -> Exp -> ThreadM Exp
thread_transX_enter table xx
 = case xx of
 	XLAM b k x
	 -> do	pushWitnessVK (varOfBind b) k
	 	return	xx

	XLocal r vts x
	 -> do	mapM_ (\(v, k) -> pushWitnessVK v k)
	 		[ (v, kindOfType t)	| (v, t) <- vts]

 		return xx

	_ 	-> return xx		


-- | If this is an explicitly constructed witness then try and replace it by
--	a variable which binds the correct one.
--
rewriteWitness :: ClassInstMap -> Type -> ThreadM Type
rewriteWitness instMap tt
 
	-- got an application of an explicit witness to some region class
	| TClass vC [TVar k vT] 	<- tt
	= do	Just vW	<- lookupWitness vT vC
		return $ TVar (kindOfType tt) vW

	-- purity of no effects is trivial
	| TClass vC [TBot KEffect]	<- tt
	, vC == primPure
	= return tt

	-- ignore other purity witnesses for now
	| TClass vC [eff]		<- tt
	, vC == primPure
	= do	w	<- buildPureWitness eff
		return	$ w

	-- leave shape witnesses for Core.Reconstruct to worry about
	| TClass vC _			<- tt
	, Var.FShape{}			<- Var.bind vC
	= return tt

	-- leave type classes for Core.Dict to worry about
	| TClass vC _			<- tt
	, Just _			<- Map.lookup vC instMap
	= return tt

	-- some other witness we don't handle
	| TClass vC ts			<- tt
	= freakout stage
		("thread_transX: don't have a witness for " % tt % "\n")
		$ return tt

	-- some other type
	| otherwise
	= return tt



-- | Build a witness that this effect is pure.
buildPureWitness
	:: Effect
	-> ThreadM Witness

buildPureWitness eff@(TEffect vE [tR@(TVar KRegion vR)])
	| vE == primRead
	= do	Just wConst	<- lookupWitness vR primConst
		return		$ TPurify eff (TVar (KClass primConst [tR]) wConst)

buildPureWitness eff@(TSum KEffect _)
 = do	let effs	= flattenTSum eff
 	
	wits		<- mapM buildPureWitness effs
	return		$ TPurifyJoin wits

buildPureWitness eff@(TVar KEffect vE)
 = do	Just w	<- lookupWitness vE primPure
 	return (TVar (KClass primPure [eff]) w)

buildPureWitness eff
 = panic stage
 	$ "buildPureWitness: Cannot build a witness for purity of " % eff % "\n"



-- | Lookup a witness for a constraint on this region
lookupWitness 
	:: Var 		-- ^ the data/region/effect variable
	-> Var		-- ^ the witness needed
	-> ThreadM (Maybe Var)
	
lookupWitness vRegion vClass
 = do
 	state	<- get
	
	let mvWitness
		-- we've got a witness for this class in the table.
		| Just vWitness	<- lookup (vClass, vRegion) state
		= Just vWitness

		-- uh oh, we don't have a witness for this one.
		-- Print an error instead of panicing so we can still drop the file for -dump-core-thread
		--	Core.Reconstruct will catch this problem in a subsequent stage.
		--
	 	| otherwise
	 	= freakout stage
			("thread_transX: can't find a witness for " % vClass % " on region " % vRegion % "\n")
			$ Nothing

	return mvWitness

-- Inspect this kind. If it binds a witness then record it in the state.
pushWitnessVK :: Var -> Kind -> ThreadM ()
pushWitnessVK vWitness k
 	| KClass vClass [TVar kV vRE]	<- k
	, elem kV [KRegion, KEffect, KData]
	= modify $ \s -> ((vClass, vRE), vWitness) : s
	
	| otherwise
	= return ()


-- Inspect this kind. If it binds a witness then pop it from the stack.
popWitnessVK :: Var -> Kind -> ThreadM ()
popWitnessVK vWitness k
	| KClass vClass [TVar kV vRE]	<- k
	= do
		state	<- get

		let xx	| (c : cs)			<- state
			, c == ((vClass, vRE), vWitness)
			= put cs
			
			| otherwise
			= panic stage
			$ "popWitnessVK: witness to be popped does not match.\n"
			
		xx

	| otherwise
	= return ()
	
