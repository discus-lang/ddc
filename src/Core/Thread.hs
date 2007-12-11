
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
	= Map 	Var		-- region variable
		[(Var, Var)]	-- class name, witness variable for this class

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
		 { bindK	= thread_bindK 
		 , bindT	= thread_bindT
		 , transX 	= thread_transX instMap }
	
   in	evalState (walkZM walkTable sTree) Map.empty

-- | Slurp out all the class instances from ths tree
slurpClassInstMap
 ::	Tree	-> Map Var [Top]

slurpClassInstMap tree
 = 	Map.gather
 	[ (v, p)	| p@(PClassInst v ts context defs) <- tree]


-- | top-down: bind kinds and remember witnesses
--	This will catch witnesses bound via XLAM
--
thread_bindK :: WalkTable ThreadM -> Var -> Kind -> ThreadM (WalkTable ThreadM)
thread_bindK table v k
 = do	
 	-- add the kind to the regular walk table
 	let table'	= table { boundK = Map.insert v k $ boundK table }
	
	-- if this binds a witness then record it in the state
	addWitnessVK v k
	
 	return table'

	
-- | top-down: bind types and remember witnesses
--	This will catch witnesses created by XLocal
thread_bindT :: WalkTable ThreadM -> Var -> Type -> ThreadM (WalkTable ThreadM)
thread_bindT table v t
 = do
 	-- add the type to the regular walk table
 	let table'	= table { boundT = Map.insert v t $ boundT table }
	
	-- if this bind type is a witnesses then record it in the state
	addWitnessVK v (kindOfType t)
	
	return table'

	
-- | bottom-up: replace applications of static witnesses with bound witness variables.
thread_transX :: ClassInstMap -> WalkTable ThreadM -> Exp -> ThreadM Exp
thread_transX instMap table xx
 = case xx of
 	XAPP x t@(TClass{})
	 -> do	t'	<- rewriteWitness instMap t
	 	return	$ XAPP x t'
		
	_ ->	return xx
	

-- | If this is an explicitly constructed witness then try and replace it by
--	a variable which binds the correct one.
--
rewriteWitness :: ClassInstMap -> Type -> ThreadM Type
rewriteWitness instMap tt
 
	-- got an application of an explicit witness to some region class
	| TClass vC [TVar KRegion vR] 	<- tt
	= do	state	<- get
		
		let tt'	
			-- we've got a witness for this class in the table.
			| Just classes	<- Map.lookup vR state
			, Just w	<- lookup vC classes
			= TVar KWitness w
		 
			-- uh oh, we don't have a witness for this one.
			-- Print an error instead of panicing so we can still drop the file for -dump-core-thread
			--	Core.Reconstruct will catch this problem in a subsequent stage.
			--
		 	| otherwise
		 	= freakout stage
				("thread_transX: can't find a witness for " % tt % "\n")
				$ tt
		
		return tt'

	-- purity of no effects is trivial
	| TClass vC [TBot KEffect]	<- tt
	, vC == primPure
	= return tt

	-- ignore other purity witnesses for now
	| TClass vC _			<- tt
	, vC == primPure
	= return tt

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


-- Inspect this kind. If it binds a witness then record it in the state.
addWitnessVK :: Var -> Kind -> ThreadM ()
addWitnessVK vWitness k
 	| KClass vClass [TVar kV vRE]	<- k
	, elem kV [KRegion, KEffect]
	= addWitness vRE vClass vWitness
	
	| otherwise
	= return ()


-- Add a witness to some effect or property to the state
addWitness
	:: Var 	-- ^ region / effect variable
	-> Var	-- ^ class variable	(eg, Mutable)
	-> Var	-- ^ witness varaible	(eg, +w123)
	-> ThreadM ()
	
addWitness vRE vClass vWitness
 = do	modify	$ Map.alter 
			(\x -> case x of
				Nothing		-> Just [(vClass, vWitness)]
				Just ws		-> Just $ (vClass, vWitness) : ws)
			vRE
	
	return ()
	
