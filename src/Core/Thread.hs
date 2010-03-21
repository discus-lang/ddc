
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
import Core.Plate.Trans
import Core.Exp
import Type.Builtin
import Type.Util
import Type.Exp
import Shared.Var
import Shared.VarPrim
import Shared.Error
import Util
import qualified Shared.Var		as Var
import qualified Util.Data.Map		as Map


-----
stage	= "Core.Thread"


--------------------------------------------------------------------------------
-- Maintain a map of regions, to what witnesses we have available for them.
type ThreadS
	= [((TyClass, Var), Var)]	-- (class name, region), witness variable
	

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
   
	transTable
		 = transTableId
		 { transP	= thread_transP 
		 , transX 	= thread_transX instMap 
		 , transX_enter	= thread_transX_enter }
	
   in	evalState (transZM transTable sTree) []

-- | Slurp out all the class instances from ths tree
slurpClassInstMap
 ::	Tree	-> Map Var [Top]

slurpClassInstMap tree
 = 	Map.gather
 	[ (v, p)	| p@(PClassInst v ts context defs) <- tree]


-- | push witnesses to properties of top-level regions.
thread_transP :: Top -> ThreadM Top
thread_transP pp
 = case pp of
 	PRegion r vts
	 -> do	mapM_ (\(v, t) -> let Just k = kindOfType t in pushWitnessVK v k) vts
	 	return pp

	_ -> return pp


-- | bottom-up: replace applications of static witnesses with bound witness variables.
thread_transX :: ClassInstMap -> Exp -> ThreadM Exp
thread_transX instMap xx
	| XAPP x t		<- xx
	, Just _		<- takeTWitness t
	= do	t'	<- rewriteWitness instMap t
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
rewriteWitness :: ClassInstMap -> Type -> ThreadM Type
rewriteWitness instMap tt
 = case tt of
	-- handle compound witnesses
	TWitJoin ts
	 -> do	ts'	<- mapM (rewriteWitness' instMap) ts
		return	$ makeTWitJoin ts'

	_	-> rewriteWitness' instMap tt

rewriteWitness' instMap tt
 
	-- Got an application of an explicit witness to some region.
	--	Lookup the appropriate witness from the environment and use
	--	that here.
	| Just (vC, _, [TVar k vT]) 	<- mClass
	= do	Just vW	<- lookupWitness vT vC
		let Just k	= kindOfType tt
		return $ TVar k vW

	-- purity of no effects is trivial
	| Just (TyClassPure, _, [TBot kE])	<- mClass
	, kE	== kEffect
	= return tt

	-- empty of no closure is trivial
	| Just (TyClassEmpty, _, [TBot kC]) <- mClass
	, kC	== kClosure
	= return tt

	-- build a witness for purity of this effect
	| Just (TyClassPure, _, [eff])	<- mClass
	= do	w	<- buildPureWitness eff
		return	$ w

	-- leave shape witnesses for Core.Reconstruct to worry about
	| Just (TyClass vC, _, _)	<- mClass
	, Var.FShape{}			<- Var.bind vC
	= return tt

	-- leave user type classes for Core.Dict to worry about
	| Just (TyClass vC, _, _)	<- mClass
	, Just _			<- Map.lookup vC instMap
	= return tt

	-- function types are always assumed to be lazy, 
	--	Lazy witnesses on them are trivially satisfied.
	| Just (TyClassLazyH, _, [t1])		<- mClass
	, isJust (takeTFun t1)
	= return tt

	| Just (TyClassLazyH, _, [TFetters t1 _]) <- mClass
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
		Just wConst	<- lookupWitness vR TyClassConst
		let  kConst	= KClass TyClassConst [tR]

		-- the purity witness gives us purity of read effects on that const region
		return		$ TApp (TApp (TCon tcPurify) tR) (TVar kConst wConst)

buildPureWitness eff@(TSum kE _)
 | kE	== kEffect
 = do	let effs	= flattenTSum eff
 	wits		<- mapM buildPureWitness effs
	return		$ TWitJoin wits

buildPureWitness eff@(TVar kE vE)
 | kE	== kEffect
 = do	Just w	<- lookupWitness vE TyClassPure
 	return (TVar (KClass TyClassPure [eff]) w)

buildPureWitness eff
 = panic stage
 	$ "buildPureWitness: Cannot build a witness for purity of " % eff % "\n"



-- | Lookup a witness for a constraint on this region
lookupWitness 
	:: Var 		-- ^ the data\/region\/effect variable
	-> TyClass	-- ^ the witness needed
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
			("thread_transX: can't find a witness for " % vClass % " on region " % vRegion % "\n"
			 % "state = " % state % "\n\n")
			$ Nothing

	return mvWitness

-- Inspect this kind. If it binds a witness then record it in the state.
pushWitnessVK :: Var -> Kind -> ThreadM ()
pushWitnessVK vWitness k
 	| KClass vClass [TVar kV vRE]	<- k
	, elem kV [kRegion, kEffect, kValue, kClosure]
	= modify $ \s -> ((vClass, vRE), vWitness) : s
	
	| otherwise
	= return ()


-- Inspect this kind. If it binds a witness then pop it from the stack.
popWitnessVK :: Var -> Kind -> ThreadM ()
popWitnessVK vWitness k
	| KClass vClass [TVar kV vRE]	<- k
	= do
		state	<- get

		let (xx	:: ThreadM ())
			| (c : cs)			<- state
			, c == ((vClass, vRE), vWitness)
			= put cs
			
			| otherwise
			= panic stage
			$ "popWitnessVK: witness to be popped does not match.\n"
			
		xx

	| otherwise
	= return ()
	
