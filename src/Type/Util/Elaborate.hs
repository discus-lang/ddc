
-- Handles elaboration of types for the ffi.
--	Adding all the regions, effects and closures to a type by hand is boring and error prone.
--	Luckilly, for most functions this information is fairly unsurprising. Guided by some user
--	keywords, we can add this information automatically.
--
module Type.Util.Elaborate
	( elaborateRsT, elaborateRsT_quant
	, elaborateEffT
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
import Type.Pretty
import Type.Util.Pack
import Type.Util.Bits
import Type.Util.Kind	
import Type.Plate.Collect

import qualified Debug.Trace	as Debug

-----
stage	= "Type.Util.Elaborate"
debug	= False
trace ss xx
 = if debug 
 	then Debug.trace (pprStrPlain ss) xx
	else xx
	

-----------------------
-- elaborateRegionsT
--	Check the kinds of data type constructors, and if they don't have enough region 
--	args then add new ones to make them the right kind.
--
elaborateRsT_quant
	:: Monad m
	=> (NameSpace -> m Var)	-- ^ A compuation to generate a fresh region var
	-> (Var -> m Kind)	-- ^ fn to get kind of type ctor
	-> Type 		-- ^ the type to elaborate
	-> m Type
	
elaborateRsT_quant newVar getKind tt
 = do	(t_elab, vks)	<- elaborateRsT newVar getKind tt
	let t_quant	= makeTForall_back vks t_elab

   	trace ("elaborateRsT: " % tt % "\n")
		$ return t_quant

elaborateRsT newVar getKind tt
 = do	let ?newVar	= newVar
	let ?getKind	= getKind
	elaborateRsT' tt

elaborateRsT' tt
 	| TForall b k x		<- tt
	= do	(x', vks')	<- elaborateRsT' x
		return	( tt
			, vks')

	| TFetters x fs		<- tt
	= do	(x', vks')	<- elaborateRsT' x
		return	( TFetters x' fs
			, vks')

	| TVar{}	<- tt	
	= return (tt, [])

	| TApp t1 t2 	<- tt
	= do	(t1', vks1)	<- elaborateRsT' t1
		(t2', vks2)	<- elaborateRsT' t2
		return	( TApp t1' t2'
			, vks1 ++ vks2)

	| TWild{}	<- tt
	= return (tt, [])

	| TFun t1 t2 eff clo	<- tt
	= do
		(t1', vks1)	<- elaborateRsT' t1
		(t2', vks2)	<- elaborateRsT' t2
		return	( TFun t1' t2' eff clo
			, vks1 ++ vks2)

	| TElaborate ee t	<- tt
	= do	(t', vks)	<- elaborateRsT' t
		return	( TElaborate ee t'
			, vks)

	-- add new regions to data type constructors to bring them up
	--	to the right kind.
	| TData k v ts		<- tt
	= do	kind		<- ?getKind v
		let ?topType	= tt 
		(ts2, vks2)	<- elabRs ts kind
		(ts3, vks3)	<- liftM unzip $ mapM elaborateRsT' ts2
		
		return	( TData k v ts3
			, vks2 ++ concat vks3)

	| otherwise
	= panic stage
		$ "elaborateRsT': no match for " % tt % "\n\n"
		% " tt = " % show tt % "\n"

-- | Take some arguments from a type ctor and if needed insert fresh region vars
--	so the reconstructed ctor with these args will have this kind.
--
elabRs 	:: Monad m
	=> (?newVar  :: NameSpace -> m Var)	-- ^ fn to create new region vars
	-> (?getKind :: Var -> m Kind)		-- ^ fn to get kind of type ctor
	-> (?topType :: Type)			-- ^ for debugging
	-> [Type]				-- ^ ctor args
	-> Kind					-- ^ kind to turn the ctor into
	-> m ( [Type]		-- new ctor args
	     , [(Var, Kind)])	-- added vars

elabRs args kind
  = trace ("elabRs: " % args % " " % kind % "\n")
  $ do	(args', vks)	<- elabRs2 args kind
 	return	(args', nub vks)

elabRs2 [] KValue
	= return ([], [])

elabRs2 [] (KFun k1 k2)
	-- add fresh vars at the end
	| elem k1 [KRegion, KEffect, KClosure]
	= do	(ts', vks')	<- elabRs2 [] k2
		vR		<- ?newVar (spaceOfKind k1)
		return		( TVar k1 vR : ts'
				, (vR, k1) : vks')
	| otherwise
	= return ([], [])

elabRs2 (t:ts) kk@(KFun k1 k2)

	-- (% : _)   % -> _
	| elem k1 [KRegion, KEffect, KClosure]
	, hasKind k1 t
	= do	(ts', vks')	<- elabRs2 ts k2
		return		( t : ts'
				, vks')

	-- (_ : _)   % -> _
	| elem k1 [KRegion, KEffect, KClosure]
	, not $ hasKind k1 t
	= do	vR		<- ?newVar (spaceOfKind k1)

		(tt', vksMore)	<- elabRs2 
					(TVar k1 vR : t : ts) 
					kk
		return		( tt'
				, (vR, k1) : vksMore)

	| otherwise
	= do	(ts', vks')	<- elabRs2 ts k2
		return		( t : ts'
				, vks')

elabRs2 args kind
	= panic stage
	$ "elabRs2: no match for " % (args, kind) % "\n"
	% "    topType = " % prettyTS ?topType % "\n"


hasKind k tt
 = case tt of
 	TVar k2 _	-> k == k2
	TWild k2	-> k == k2
	_		-> False



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
	| TForall b k x		<- tt
	= do	(x', fs, clo)	<- elaborateCloT' env x
		return	( TForall b k x'
			, fs
			, Nothing)
			
	-- if we see an existing set of fetters,
	--	then drop the new ones in the same place.
	| TFetters x fs		<- tt
	= do	(x', fs', mClo)	<- elaborateCloT' env x
		return	( TFetters x' (fs ++ fs')
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
			TFun{}	-> FWhere cloVarC
				$ TMask KClosure
					(makeTSum KClosure [clo, fromMaybe (TBot KClosure) mClo])
					(TTag varVal) 

			-- rhs of function isn't another function
			--	pretend that all the args are referenced here.
			_	-> FWhere cloVarC (makeTSum KClosure env)

		-- Don't add the closure variable if the fetter is just bottom.
		let (fNew, cloAnnot)
			= case fNewClo of 
				FWhere _ (TBot KClosure)	-> (Nothing, 	TBot KClosure)
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
	| TForall b k t		<- tt
	, Just (t', var, mEff)	<- hookEffT hookVar t
	= Just	( TForall b k t'
		, var
		, mEff)
	
	-- decend into fettered types
	| TFetters t fs		<- tt
	, Just (t', var, mEff)	<- hookEffT hookVar t
	= Just	( TFetters t' fs
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
 	TForall b k t1 
	 -> TForall b k (addEffectsToFsT effs var t1)

	TFetters t1 fs
	 -> TFetters t1 (addEffectsToFs var effs fs)
	 
	tx
	 -> TFetters tx [FWhere (TVar KEffect var) (makeTSum KEffect effs)]
	 

-- didn't find a fetter with this var, so add a new one
addEffectsToFs v1 effs1 []
	= [FWhere (TVar KEffect v1) (makeTSum KEffect effs1)]
	
addEffectsToFs v1 effs1 (f:fs)
 = case f of
 	FWhere (TVar KEffect v2) eff2
	 | v1 == v2	-> FWhere (TVar KEffect v2) (makeTSum KEffect (eff2 : effs1)) : fs
	
	_		-> f : addEffectsToFs v1 effs1 fs


-- | Slurp out region variables which appear in contra-variant branches
--	of this type.
slurpConRegions
	:: Type -> [Var]

slurpConRegions 
 = slurpConRegionsCo

slurpConRegionsCo tt
 = case tt of
 	TForall b k t		-> slurpConRegionsCo t
	TFetters t fs		-> slurpConRegionsCo t

	TFun t1 t2 eff clo
	 -> slurpConRegionsCon t1
	 ++ slurpConRegionsCo  t2

	TData k v ts		-> catMap slurpConRegionsCo ts

	TVar _ _			-> []
	 
slurpConRegionsCon tt
 = case tt of
 	TFun t1 t2 eff clo
	 -> slurpConRegionsCon t1
	 ++ slurpConRegionsCon t2
	 
	TData k v ts		-> catMap slurpConRegionsCon ts

	TVar KRegion v		-> [v]
	TVar _ _		-> []	
