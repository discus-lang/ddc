
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
	(threadGlob)
where
import Core.Plate.Trans
import Core.Exp
import Core.Glob
import Type.Builtin
import Type.Util
import Type.Exp
import Util
import Shared.VarPrim
import DDC.Main.Error
import DDC.Var
import qualified DDC.Var.PrimId		as Var
import qualified Data.Map		as Map

stage	= "Core.Thread"

-- Maintain a map of regions, to what witnesses we have available for them.
type ThreadS	= [((TyConWitness, Var), Var)]	-- (class name, region), witness variable
type ThreadM 	= State ThreadS

-- | Thread witness variables in this tree
threadGlob 
	:: Glob		-- ^ Header glob
	-> Glob		-- ^ Module glob
	-> Glob

threadGlob cgHeader cgModule
	= evalState (threadGlobM cgHeader cgModule) []
	

threadGlobM :: Glob -> Glob -> ThreadM Glob
threadGlobM cgHeader cgModule
 = do	
	-- Add all the top level region witnesses to the state.
	mapM_ 	(\(PRegion r vts) -> mapM addRegionWitness vts) 
		$ Map.elems 
		$ globRegion cgModule
	
	-- Thread witnesses through top level bindings.	
	let isUserClassName v
		=   (Map.member v $ globClassDict cgHeader)
		 || (Map.member v $ globClassDict cgModule)
		
	let transTable
		 = transTableId
		 { transX 	= thread_transX isUserClassName
		 , transX_enter	= thread_transX_enter }

	mapBindsOfGlobM (transZM transTable) cgModule


addRegionWitness :: (Var, Type) -> ThreadM ()
addRegionWitness (v, t)
 = do	let Just k	= kindOfType t
	pushWitnessVK v k
	return ()


-- | bottom-up: replace applications of static witnesses with bound witness variables.
thread_transX :: (Var -> Bool) -> Exp -> ThreadM Exp
thread_transX isUserClassName xx
	| XAPP x t		<- xx
	, Just _		<- takeTWitness t
	= do	t'	<- rewriteWitness isUserClassName t
	 	return	$ XAPP x t'

	-- pop lambda bound witnesses on the way back up because we're leaving their scope.
	| XLAM b k x		<- xx
	= do	popWitnessVK (varOfBind b) k
	 	return	xx

	-- pop locally bound witnesses on the way back up because we're leaving their scope.
	| XLocal r vts x	<- xx
	= do	mapM_ (\(v, k) -> popWitnessVK v k)
	 		[ (v, let Just k = kindOfType t in k)	
				| (v, t) <- reverse vts]
		
	 	return xx

	| otherwise
	= return xx


thread_transX_enter :: Exp -> ThreadM Exp
thread_transX_enter xx
 = case xx of
 	XLAM b k x
	 -> do	pushWitnessVK (varOfBind b) k
	 	return	xx

	XLocal r vts x
	 -> do	mapM_ (\(v, k) -> pushWitnessVK v k)
	 		[ (v, let Just k = kindOfType t in k)	
				| (v, t) <- vts]

 		return xx

	_ 	-> return xx		


-- | If this is an explicitly constructed witness then try and replace it by
--	a variable which binds the correct one.
--
rewriteWitness 
	:: (Var -> Bool) 
	-> Type 
	-> ThreadM Type

rewriteWitness isUserClassName tt
 = case tt of
	-- handle compound witnesses
	TWitJoin ts
	 -> do	ts'	<- mapM (rewriteWitness' isUserClassName) ts
		return	$ makeTWitJoin ts'

	_	-> rewriteWitness' isUserClassName tt

rewriteWitness' isUserClassName tt
 
	-- Got an application of an explicit witness to some region.
	--	Lookup the appropriate witness from the environment and use
	--	that here.
	| Just (vC, _, [TVar k vT]) 		<- mClass
	= do	Just vW	<- lookupWitness vT vC
		let Just k	= kindOfType tt
		return $ TVar k vW

	-- purity of no effects is trivial
	| Just (TyConWitnessMkPure, _, [TBot kE])	<- mClass
	, kE	== kEffect
	= return tt

	-- empty of no closure is trivial
	| Just (TyConWitnessMkEmpty, _, [TBot kC]) 	<- mClass
	, kC	== kClosure
	= return tt

	-- build a witness for purity of this effect
	| Just (TyConWitnessMkPure, _, [eff])		<- mClass
	= do	w	<- buildPureWitness eff
		return	$ w

	-- leave shape witnesses for Core.Reconstruct to worry about
	| Just (TyConWitnessMkVar vC, _, _)		<- mClass
	, VarIdPrim Var.FShape{}			<- varId vC
	= return tt

	-- leave user type classes for Core.Dict to worry about
	| Just (TyConWitnessMkVar vC, _, _)		<- mClass
	= return tt

	-- function types are always assumed to be lazy, 
	--	Lazy witnesses on them are trivially satisfied.
	| Just (TyConWitnessMkHeadLazy, _, [t1])		<- mClass
	, isJust (takeTFun t1)
	= return tt

	| Just (TyConWitnessMkHeadLazy, _, [TFetters t1 _]) <- mClass
	, isJust (takeTFun t1)
	= return tt
	
	-- some other witness we don't handle
	| Just (vC, _, ts)		<- mClass
	= panic stage
		("thread_transX: can't find a witness for " % tt % "\n")

	-- some other type
	| otherwise
	= return tt

	where	mClass	= takeTWitness tt


-- | Build a witness that this effect is pure.
buildPureWitness
	:: Effect
	-> ThreadM Witness

buildPureWitness eff@(TEffect vE [tR@(TVar kR vR)])
	| vE == primRead
	, kR == kRegion
	= do	
		-- try and find a witness for constness of the region
		Just wConst	<- lookupWitness vR TyConWitnessMkConst
		let  kConst	= KApps kConst [tR]

		-- the purity witness gives us purity of read effects on that const region
		return		$ TApp (TApp tMkPurify tR) (TVar kConst wConst)

buildPureWitness eff@(TSum kE _)
 | kE	== kEffect
 = do	let effs	= flattenTSum eff
 	wits		<- mapM buildPureWitness effs
	return		$ TWitJoin wits

buildPureWitness eff@(TVar kE vE)
 | kE	== kEffect
 = do	Just w	<- lookupWitness vE TyConWitnessMkPure
 	return (TVar (KApps kPure [eff]) w)

buildPureWitness eff
 = panic stage
 	$ "buildPureWitness: Cannot build a witness for purity of " % eff % "\n"



-- | Lookup a witness for a constraint on this region
lookupWitness 
	:: Var 		-- ^ the data\/region\/effect variable
	-> TyConWitness	-- ^ the witness needed
	-> ThreadM (Maybe Var)
	
lookupWitness vRegion tcWitness
 = do
 	state	<- get
	
	let mvWitness
		-- we've got a witness for this class in the table.
		| Just vWitness	<- lookup (tcWitness, vRegion) state
		= Just vWitness

		-- uh oh, we don't have a witness for this one.
		-- Print an error instead of panicing so we can still drop the file for -dump-core-thread
		--	Core.Reconstruct will catch this problem in a subsequent stage.
		--
	 	| otherwise
	 	= freakout stage
			("thread_transX: can't find a witness for " % tcWitness % " on region " % vRegion % "\n"
			 % "state = " % state % "\n\n")
			$ Nothing

	return mvWitness


-- Inspect this kind. If it binds a witness then record it in the state.
pushWitnessVK :: Var -> Kind -> ThreadM ()
pushWitnessVK vWitness k
 	| KApps (KCon kcClass _) [TVar kV vRE]	<- k
	, elem kV [kRegion, kEffect, kValue, kClosure]
	= let 	Just tcWitness	= takeTyConWitnessOfKiCon kcClass
	  in	modify $ \s -> ((tcWitness, vRE), vWitness) : s
	
	| otherwise
	= return ()


-- Inspect this kind. If it binds a witness then pop it from the stack.
popWitnessVK :: Var -> Kind -> ThreadM ()
popWitnessVK vWitness k
	| KApps (KCon kcClass _) [TVar kV vRE]	<- k
	= do
		state	<- get
		let Just tcWitness	= takeTyConWitnessOfKiCon kcClass

		let (xx	:: ThreadM ())
			| (c : cs)			<- state
			, c == ((tcWitness, vRE), vWitness)
			= put cs
			
			| otherwise
			= panic stage
			$ "popWitnessVK: witness to be popped does not match.\n"
			
		xx

	| otherwise
	= return ()
	
