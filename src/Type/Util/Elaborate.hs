
-- Handles elaboration of types for the ffi.
--	Adding all the regions, effects and closures to a type by hand is boring and error prone.
--	Luckilly, for most functions this information is fairly unsurprising. Guided by some user
--	keywords, we can add this information automatically.
--
module Type.Util.Elaborate
	( elaborateT 
	, elaborateRegionsT
	, elaborateCloT )
where

-----
import Util

-----
import qualified Shared.Var as Var
import Shared.Var 	(Var, NameSpace(..))
import Shared.VarPrim
import Shared.Pretty
import Shared.Error 

import Type.Exp
import Type.Pretty		()
import Type.Util.Pack
import Type.Util.Bits		
import Type.Plate.Collect

import qualified Debug.Trace	as Debug

-----
stage	= "Type.Elaborate"
debug	= False
trace ss xx
 = if debug 
 	then Debug.trace (pprStrPlain ss) xx
	else xx
	
-- | Elaborate this type
--
--	* Fresh, forall bound region variables are applied to type constructors
--	  of higher kind so that the result is something of kind data.
--
--	* For all regions marked Mutable, an appropriate context is added to the
--	  front of the type.
--
--	* For all regions added this way, an effect which Reads them is added
--	  to the right most function arrow in the type. In addition, if they were 
--	  marked as mutable an effect which Writes to them is also added.
--
elaborateT 
	:: Monad m
	=> (?newVarN 	:: NameSpace 	-> m Var)
	-> (?getKind	:: Var		-> m Kind)
	-> Type -> m Type

elaborateT t
 = do	
 	-- elaborate regions
 	(tRegions, vksRsConst, vksRsMutable)	
 			<- elaborateRegionsT (?newVarN NameRegion) ?getKind t

	-- if the type is a function then elaborate its closure and effect as well.
	case tRegions of
	 TData{}	
	  -> return tRegions

	 _		
	  -> do	tClosure	<- elaborateCloT tRegions
		tEffect		<- elaborateEffT (map fst vksRsConst) (map fst vksRsMutable) tClosure
	 	return	$ makeTForall_back (vksRsConst ++ vksRsMutable)
			$ tEffect


-----------------------
-- elaborateRegionsT
--	Check the kinds of data type constructors, and if they don't have enough region 
--	args then add new ones to make them the right kind.
--
elaborateRegionsT
	:: Monad m
	=> (m Var)		-- ^ A compuation to generate a fresh region var
	-> (Var -> m Kind)	-- ^ fn to get kind of type ctor
	-> Type 		-- ^ the type to elaborate
	-> m ( Type		-- new type
	     , [(Var, Kind)]	-- extra constant regions
	     , [(Var, Kind)])	-- extra mutable regions

elaborateRegionsT newVar getKind tt
 = let	?newVar		= newVar
 	?getKind	= getKind
   in	elaborateRegionsT' tt

elaborateRegionsT' tt
 = do	(tt', vksConst, vksMutable, fs)	<- elaborateRegionsT2 tt
	return	( addFetters fs tt'
		, vksConst
		, vksMutable)

elaborateRegionsT2 tt

	-- if we see a forall then drop new regions on that quantifier
 	| TForall vks x		<- tt
	= do	(x', vksC', vksM', fs)	<- elaborateRegionsT2 x
		return	( makeTForall_back (vksC' ++ vksM') (TForall vks x')
			, []
			, []
			, fs)

	| TFetters fs x		<- tt
	= do	(x', vksC, vksM, fs')	<- elaborateRegionsT2 x
		return	( TFetters (fs ++ fs') x'
			, vksC
			, vksM
			, [])

	| TVar{}		<- tt
	=	return 	( tt, [], [], [])

	| TWild{}		<- tt
	=	return	( tt, [], [], [])

	| TFun t1 t2 eff clo	<- tt
	= do
		(t1', vksC1, vksM1, fs1)	<- elaborateRegionsT2 t1
		(t2', vksC2, vksM2, fs2)	<- elaborateRegionsT2 t2
		return	( TFun t1' t2' eff clo
			, vksC1 ++ vksC2 
			, vksM1 ++ vksM2
			, fs1 ++ fs2)

	-- assume every region under a mutable operator is mutable
	| TMutable x		<- tt
	= do	(x', vksC, vksM, fs1)	<- elaborateRegionsT2 x

		-- collect up all the vars in the rewritten type and choose all the regions
		let vsRegions	= filter (\v -> Var.nameSpace v == NameRegion)
				$ collectVarsT x'

		-- build a Mutable constraint for these regions
		let fNew	= FConstraint primMutable
					[ makeTSum KRegion $ map (TVar KRegion) vsRegions ]

		return	( x'
			, []
			, vksC ++ vksM
			, fNew : fs1)

	-- add new regions to data type constructors to bring them up
	--	to the right kind.
	| TData v ts		<- tt
	= do	kind		<- ?getKind v

		trace ("elaborateRegionsT: " % v % " :: " % kind % "\n")
			$ return ()
		
		(ts2, vks2)	<- elabRs ts kind
		(ts3, vksC3, vksM3, fs3)	
				<- liftM unzip4 $ mapM elaborateRegionsT2 ts2
		
		return	( TData v ts3
			, vks2 ++ concat vksC3
			, concat vksM3
			, concat fs3)


-- | Take some arguments from a type ctor and if needed insert fresh region vars
--	so the reconstructed ctor with these args will have this kind.
--
elabRs 	:: Monad m
	=> (?newVar  :: m Var)			-- ^ fn to create new region vars
	-> (?getKind :: Var -> m Kind)		-- ^ fn to get kind of type ctor
	-> [Type]				-- ^ ctor args
	-> Kind					-- ^ kind to turn the ctor into
	-> m ( [Type]		-- new ctor args
	     , [(Var, Kind)])	-- added vars

elabRs args kind
-- = trace ("elabRs: " % args % " " % kind % "\n")
  = do	(args', vks)	<- elabRs2 args kind
 	return	(args', nub vks)

elabRs2 [] KData
	= return ([], [])

elabRs2 [] (KFun k1 k2)
	| KRegion		<- k1
	= do	(ts', vks')	<- elabRs2 [] k2
		vR		<- ?newVar
		return		( TVar KRegion vR : ts'
				, (vR, KRegion) : vks')

elabRs2 (t:ts) kk@(KFun k1 k2)

	-- (% : _)   % -> _
	| KRegion		<- k1
	, KRegion		<- let Just k = takeKindOfType t in k
	= do	(ts', vks')	<- elabRs2 ts k2
		return		( t : ts'
				, vks')

	-- (_ : _)   % -> _
	| KRegion		<- k1
	, k			<- let Just k = takeKindOfType t in k
	, k /= KRegion
	= do	vR		<- ?newVar

		(tt', vksMore)	<- elabRs2 
					(TVar KRegion vR : t : ts) 
					kk
		return		( tt'
				, (vR, KRegion) : vksMore)

	| otherwise
	= do	(ts', vks')	<- elabRs2 ts k2
		return		( t : ts'
				, vks')

elabRs2 args kind
	= panic stage
	$ "elabRs2': no match for " % (args, kind) % "\n"


-----------------------
-- elaborateCloT
--	Add closure annotations on function constructors, assuming 
--	that the body of the function references all it's arguments.
--
-- eg	   (a -> b -> c -> d)
--
--	=> (t1 -> t2 -($c1)> t3 -($c2)> t4)
--	    :- $c1 = $c2 \ x2
--	    ,  $c2 = { x1 : t1; x2 : t2 }

elaborateCloT 
	:: Monad m
	=> (?newVarN :: NameSpace -> m Var)	-- ^ fn to use to create new vars
	-> Type					-- ^ the type to elaborate 
	-> m Type				-- elaborated type

elaborateCloT tt
 = do	(tt', fs, _)	<- elaborateCloT' [] tt
  	return	$ addFetters fs tt'
	
elaborateCloT' env tt
	| TForall vks x		<- tt
	= do	(x', fs, clo)	<- elaborateCloT' env x
		return	( TForall vks x'
			, fs
			, Nothing)
			
	-- if we see an existing set of fetters,
	--	then drop the new ones in the same place.
	| TFetters fs x		<- tt
	= do	(x', fs', mClo)	<- elaborateCloT' env x
		return	( TFetters (fs ++ fs') x'
			, []
			, mClo)
			
	| TVar{}		<- tt
	= 	return	( tt
			, []
			, Nothing )
	
	-- TODO: decend into type ctors
	| TData{}		<- tt
	=	return	( tt
			, []
			, Nothing)

	| TFun t1 t2 eff clo	<- tt
	= do	
		-- create a new var to label the arg
		varVal		<- ?newVarN NameValue
		
		-- elaborate the right hand arg,  carrying the new argument down into it
		let argClo	= TFree varVal t1
		(t2', fs, mClo)	<- elaborateCloT' (env ++ [argClo]) t2

		-- the closure for this function is the body minus the var for the arg
		varC		<- ?newVarN NameClosure
		let cloVarC	= TVar KClosure varC

		let fNewClo	=
		     case t2 of
			-- rhs of this function is another function
			--	set this closure to be closure of rhs without the arg bound here
			TFun{}	-> FLet cloVarC
				$ TMask KClosure
					(makeTSum KClosure [clo, fromMaybe (TBot KClosure) mClo])
					(TTag varVal) 

			-- rhs of function isn't another function
			--	pretend that all the args are referenced here.
			_	-> FLet cloVarC (makeTSum KClosure env)

		-- Don't add the closure variable if the fetter is just bottom.
		let (fNew, cloAnnot)
			= case fNewClo of 
				FLet _ (TBot KClosure)	-> (Nothing, 	TBot KClosure)
				_			-> (Just fNewClo, cloVarC)

		return	( TFun t1 t2' eff cloAnnot
			, maybeToList fNew ++ fs
			, Just cloAnnot)


-- | Elaborate effects in this type.
elaborateEffT 
	:: Monad m
	=> (?newVarN :: NameSpace -> m Var)
	-> [Var]		-- ^ region variables which were added during elaboration.
	-> [Var]
	-> Type			-- ^ the type to elaborate
	-> m Type
	
elaborateEffT vsRsConst vsRsMutable tt
 = do	
 	-- make a fresh hook var.
 	freshHookVar	<- ?newVarN NameEffect

	-- see if there is already a var on the rightmost function arrow, if there isn't one
	--	then add the freshHookVar.
 	let Just (tHooked, hookVar, hookEffs) =
		hookEffT freshHookVar tt
  
    	-- assume that regions added into a contra-variant branch during elaboration 
	--	will be read by the function.
	let rsContra	= slurpConRegions tHooked
	let effsRead	= [ TEffect primRead [TVar KRegion v] 
				| v <- (vsRsConst ++ vsRsMutable)
				, elem v rsContra ]

	let effsWrite	= [ TEffect primWrite [TVar KRegion v] 
				| v <- vsRsMutable
				, elem v rsContra ]
	
	let effs	= effsRead ++ effsWrite ++ maybeToList hookEffs
	
	let tFinal	= addEffectsToFsT effs hookVar tHooked
  
 	return $ packType_noLoops tFinal


-- | Find the right most function arrow in this function type and return the effect variable
--	on it, or add this hookVar if there isn't one.
--
--	If the right most function arrow has a manifest effect on it, then change it to
--		a var and return the original effect.
--
hookEffT  
	:: Var 			-- The hook var to use if there isn't one already in the type.
	-> Type 		-- The type to change.
	-> Maybe 
		( Type		-- Hooked type
		, Var		-- Hook var to use.
		, Maybe Effect)	-- Any effects that were already on the right most function arrow.


hookEffT hookVar tt
	-- decend into foralls
	| TForall vks t		<- tt
	, Just (t', var, mEff)	<- hookEffT hookVar t
	= Just	( TForall vks t'
		, var
		, mEff)
	
	-- decend into fettered types
	| TFetters fs t		<- tt
	, Just (t', var, mEff)	<- hookEffT hookVar t
	= Just	( TFetters fs t'
		, var
		, mEff)

	-- keep decending so long as the result type is also a function.
 	| TFun t1 t2 eff clo	<- tt
	, TFun{}		<- t2
	, Just (t2', var, mEff)	<- hookEffT hookVar t2
	= Just 	( TFun t1 t2' eff clo
	  	, var
		, mEff)
	
	-- There is already a var on the right most function
	--	so we can use that as a hook var.
	| TFun t1 t2 (TVar KEffect var) clo	<- tt
	= Just	( tt
		, var
		, Nothing )
	
	-- The right-most function has no effects on it.
	--	Add the hook var that we were given.
	| TFun t1 t2 (TBot KEffect) clo		<- tt
	= Just	( TFun t1 t2 (TVar KEffect hookVar) clo
	  	, hookVar
		, Nothing )
	
	-- The right-most function has some other effect on it. 
	--	Replace this with our hook var and return the effect.
	| TFun t1 t2 eff clo			<- tt
	= Just	( TFun t1 t2 (TVar KEffect hookVar) clo
		, hookVar
		, Just eff)
	
	| otherwise
	= freakout stage
		("hookEffT: can't hook type (" % tt % ")")
		Nothing


-- | Add some effects to the fetter with this var.
--
addEffectsToFsT
	:: [Effect] -> Var -> Type -> Type
	
addEffectsToFsT effs var tt
 = case tt of
 	TForall vks t1 
	 -> TForall vks (addEffectsToFsT effs var t1)

	TFetters fs t1
	 -> TFetters (addEffectsToFs var effs fs) t1
	 
	tx
	 -> TFetters [FLet (TVar KEffect var) (makeTSum KEffect effs)] tx
	 

-- didn't find a fetter with this var, so add a new one
addEffectsToFs v1 effs1 []
	= [FLet (TVar KEffect v1) (makeTSum KEffect effs1)]
	
addEffectsToFs v1 effs1 (f:fs)
 = case f of
 	FLet (TVar KEffect v2) eff2
	 | v1 == v2	-> FLet (TVar KEffect v2) (makeTSum KEffect (eff2 : effs1)) : fs
	
	_		-> f : addEffectsToFs v1 effs1 fs


-- | Slurp out region variables which appear in contra-variant branches
--	of this type.
slurpConRegions
	:: Type -> [Var]

slurpConRegions 
 = slurpConRegionsCo

slurpConRegionsCo tt
 = case tt of
 	TForall vks t		-> slurpConRegionsCo t
	TFetters fs t		-> slurpConRegionsCo t

	TFun t1 t2 eff clo
	 -> slurpConRegionsCon t1
	 ++ slurpConRegionsCo  t2

	TData v ts		-> catMap slurpConRegionsCo ts

	TVar _ _			-> []
	 
slurpConRegionsCon tt
 = case tt of
 	TFun t1 t2 eff clo
	 -> slurpConRegionsCon t1
	 ++ slurpConRegionsCon t2
	 
	TData v ts		-> catMap slurpConRegionsCon ts

	TVar KRegion v		-> [v]
	TVar _ _		-> []	
