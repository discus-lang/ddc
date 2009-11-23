
-- | Elaboration of types for the ffi.
--   When importing foreign functions, adding all the region effects and closures 
--	annotations to a type by hand is boring and error prone. Luckily, for most 
--	functions this information is fairly unsurprising. So we can guess most
--	of it automatically.
--
module Type.Util.Elaborate
	( elaborateRsT
	, elaborateRsT_quant
	, elaborateEffT
	, elaborateCloT )
where

import Type.Exp
import Type.Pretty
import Type.Util.Pack
import Type.Util.Bits
import Type.Util.Kind	
import Type.Plate.Collect

import qualified Shared.Var as Var
import Shared.Var 	(Var, NameSpace(..))
import Shared.VarPrim
import Shared.Pretty
import Shared.Error 

import qualified Data.Set	as Set
import Data.Set			(Set)

import Util
import qualified Debug.Trace	as Debug

-----
stage	= "Type.Util.Elaborate"
debug	= False
trace ss xx
 = if debug 
 	then Debug.trace (pprStrPlain ss) xx
	else xx
	


-- Elaborate Regions ------------------------------------------------------------------------------

-- | Look at uses of data type constructors, and if they don't have enough
--	region args applied then add some more so the resulting type
--	has kind *.
elaborateRsT_quant
	:: Monad m
	=> (NameSpace -> m Var)	-- ^ A compuation to generate a fresh region var
	-> Type 		-- ^ the type to elaborate
	-> m Type		--   elaborated type.
	
elaborateRsT_quant newVar tt
 = do	(t_elab, vks)	<- elaborateRsT newVar tt
	let t_quant	= makeTForall_back vks t_elab

   	trace ("elaborateRsT: " % tt % "\n")
		$ return t_quant

elaborateRsT newVar tt
 = do	let ?newVar	= newVar
	elaborateRsT' tt

elaborateRsT' tt
 = trace ("elaborateRsT' " % tt)
 $ case tt of
	TVar{}	-> return (tt, [])
	TTop{}	-> return (tt, [])
	TBot{}	-> return (tt, [])

	TForall b k x	
	 -> do	(x', vks')	<- elaborateRsT' x
		return	( tt
			, vks')

	TFetters x fs
	 -> do	(x', vks')	<- elaborateRsT' x
		return	( TFetters x' fs
			, vks')

	
	TCon{}	
	 -- some data constructor that isn't applied to any region vars.
   	 | Just (v, kind, ts)	<- takeTData tt
 	 -> elaborateRsT_data tt (v, kind, ts)

 	 -- a function constructor don't need regions added.
	 | otherwise
	 -> return (tt, [])

	TApp t1 t2 

	 -- add new regions to data type constructors to bring them up
	 --	to the right kind.
	 | Just (v, kind, ts)	<- takeTData tt
	 -> trace ("elaborateRsT' elaborating data " % show tt % "\n")
	     $ elaborateRsT_data tt (v, kind, ts)	
	
	 | otherwise
	 -> trace ("elaborateRsT' elaborating app " % show tt % "\n")
	     $ do	(t1', vks1)	<- elaborateRsT' t1
			(t2', vks2)	<- elaborateRsT' t2
			return	( TApp t1' t2'
				, vks1 ++ vks2)

	TElaborate ee t	
	 -> do	(t', vks)	<- elaborateRsT' t
		return	( TElaborate ee t'
			, vks)

	TEffect{}	-> return (tt, [])

	_ -> panic stage
		$ "elaborateRsT': no match for " % tt % "\n\n"
		% " tt = " % show tt % "\n"

elaborateRsT_data tt (v, kind, ts)
 = do	let ?topType	= tt 
	(ts2, vks2)	<- elabRs ts kind
	(ts3, vks3)	<- liftM unzip $ mapM elaborateRsT' ts2

	return	( makeTData v kind ts3
		, vks2 ++ concat vks3)


-- | Take some arguments from a type ctor and if needed insert fresh region vars
--	so the reconstructed ctor with these args will have this kind.
elabRs 	:: Monad m
	=> (?newVar  :: NameSpace -> m Var)	-- ^ fn to create new region vars
	-> (?topType :: Type)			-- ^ for debugging
	-> [Type]				-- ^ ctor args
	-> Kind					-- ^ kind to turn the ctor into
	-> m ( [Type]		-- new ctor args
	     , [(Var, Kind)])	-- added vars

elabRs args kind
  = trace ("elabRs: " % args % " " % kind % "\n")
  $ do	(args', vks)	<- elabRs2 args kind
 	return	(args', nub vks)

elabRs2 [] k
	| k == kValue
	= return ([], [])

elabRs2 [] (KFun k1 k2)
	-- add fresh vars at the end
	| elem k1 [kRegion, kEffect, kClosure]
	= do	(ts', vks')		<- elabRs2 [] k2
		let Just nameSpace	= spaceOfKind k1
		vR			<- ?newVar nameSpace
		return	( TVar k1 vR : ts'
			, (vR, k1) : vks')
	| otherwise
	= return ([], [])

elabRs2 (t:ts) kk@(KFun k1 k2)

	-- (% : _)   % -> _
	| elem k1 [kRegion, kEffect, kClosure]
	, hasKind k1 t
	= do	(ts', vks')	<- elabRs2 ts k2
		return		( t : ts'
				, vks')

	-- (_ : _)   % -> _
	| elem k1 [kRegion, kEffect, kClosure]
	, not $ hasKind k1 t
	= do	let Just nameSpace	= spaceOfKind k1
		vR			<- ?newVar nameSpace

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
	TBot k2		-> k == k2
	_		-> False



-- Elaborate Closures -----------------------------------------------------------------------------

-- Add closure annotations on function constructors, assuming 
-- that the body of the function references all it's arguments.
--
-- eg	   (a -> b -> c -> d)
--
--	=> (t1 -> t2 -($c1)> t3 -($c2)> t4)
--	    :- $c1 = { x1 : x1 }
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
 = case tt of
	TForall b k x
	 -> do	(x', fs, clo)	<- elaborateCloT' env x
		return	( TForall b k x'
			, fs
			, tEmpty)
			
	-- if we see an existing set of fetters,
	--	then drop the new ones in the same place.
	TFetters x fs		
	 -> do	(x', fs', mClo)	<- elaborateCloT' env x
		return	( TFetters x' (fs ++ fs')
			, []
			, mClo)
	
	TCon{}
	 ->	return  ( tt
			, []
			, tEmpty)
			
	TVar{}
	 -> 	return	( tt
			, []
			, tEmpty )

	TApp ta tb
	 | Just (t1, t2, eff, clo)	<- takeTFun tt
	 -> do	-- create a new value variable as a name for the function parameter
		varVal			<- ?newVarN NameValue
		
		-- elaborate the right hand arg, 
		--	carrying the new parameter name down into it.
		let argClo		= TFree varVal t1
		(t2', fs, cloT2)	<- elaborateCloT' (env ++ [argClo]) t2

		-- make a new closure var to name the closure of this function
		varC			<- ?newVarN NameClosure
		let cloVarC		= TVar kClosure varC

		let newClo	= 
		     case t2 of
			-- if the right of the function is another function then 
			--   the closure of _this_ one is the same as the closure
			--   of the inner one, minus the parameter bound here.
			TApp{}
			 | Just _	<- takeTFun t2
			 -> dropTFreesIn (Set.singleton varVal) cloT2

			-- if the right of the function isn't another function
			--   then assume this one binds all the args.
			_	-> makeTSum kClosure env
		
		-- fetter for the closure of this function
		let fNewClo	= FWhere cloVarC newClo
		
		-- don't annotate the fn with a closure variable if the closure
		--	in the constraint is just Bot.
		let (fNew, cloAnnot)
			= case fNewClo of 
				FWhere _ (TBot kClosure)	-> (Nothing, TBot kClosure)
				_				-> (Just fNewClo, cloVarC)

		return	( makeTFun t1 t2' eff cloAnnot
			, maybeToList fNew ++ fs
			, newClo)

	 | otherwise
	 -> return 	( tt
			, []
			, tEmpty)


-- Elaborate Effects ------------------------------------------------------------------------------

-- | Elaborate effects in this type.
elaborateEffT 
	:: Monad m
	=> (?newVarN :: NameSpace -> m Var)
	-> [Var]		-- ^ constant region variables to make effects for
	-> [Var]		-- ^ mutable  region variables to make effects for
	-> Type			-- ^ the type to elaborate
	-> m Type		--   the elaborated type
	
elaborateEffT vsRsConst vsRsMutable tt
 | Nothing	<- kindOfType tt
 = panic stage "elaborateEffT: type to elaborate is not well kinded"

 | otherwise
 = do	
 	-- make a fresh hook var
	--	The new effect fetter constrains this var.
 	freshHookVar	<- ?newVarN NameEffect

	-- see if there is already a var on the rightmost function arrow,
	--	if there isn't one then add the freshHookVar.
 	let Just (tHooked, hookVar, hookEffs) =
		hookEffT freshHookVar tt
  
    	-- assume that regions added into a contra-variant
	--	branch during elaboration will be read by the function.
	let rsContra	= slurpConRegions tHooked
	let effsRead	= [ TEffect primRead [TVar kRegion v] 
				| v <- (vsRsConst ++ vsRsMutable)
				, elem v rsContra ]

	let effsWrite	= [ TEffect primWrite [TVar kRegion v] 
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
	:: Var 			-- ^ the hook var to use if there isn't one already in the type.
	-> Type 		-- ^ tThe type to change.
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
 	| Just (t1, t2, eff, clo)	<- takeTFun tt
	, Just _			<- takeTFun t2
	, Just (t2', var, mEff)		<- hookEffT hookVar t2
	= Just 	( makeTFun t1 t2' eff clo
	  	, var
		, mEff)
	
	-- There is already a var on the right most function
	--	so we can use that as a hook var.
	| Just (t1, t2, (TVar kEffect var), clo)
						<- takeTFun tt
	= Just	( tt
		, var
		, Nothing )
	
	-- The right-most function has no effects on it.
	--	Add the hook var that we were given.
	| Just (t1, t2, TBot kEffect, clo)	<- takeTFun tt
	= Just	( makeTFun t1 t2 (TVar kEffect hookVar) clo
	  	, hookVar
		, Nothing )
	
	-- The right-most function has some other effect on it. 
	--	Replace this with our hook var and return the effect.
	| Just (t1, t2, eff, clo)		<- takeTFun tt
	= Just	( makeTFun t1 t2 (TVar kEffect hookVar) clo
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
	 -> TFetters tx [FWhere (TVar kEffect var) (makeTSum kEffect effs)]
	 

-- didn't find a fetter with this var, so add a new one
addEffectsToFs v1 effs1 []
	= [FWhere (TVar kEffect v1) (makeTSum kEffect effs1)]
	
addEffectsToFs v1 effs1 (f:fs)
 = case f of
 	FWhere (TVar k v2) eff2
		| k == kEffect
		, v1 == v2	
	 	-> FWhere (TVar kEffect v2) (makeTSum kEffect (eff2 : effs1)) : fs
	
	_	-> f : addEffectsToFs v1 effs1 fs


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

	TApp{}
	 | Just (t1, t2, eff, clo)	<- takeTFun tt
	 -> slurpConRegionsCon t1
	 ++ slurpConRegionsCo  t2
	
	 | Just (v, k, ts)		<- takeTData tt
	 -> catMap slurpConRegionsCo ts

	TVar _ _			-> []
	 
slurpConRegionsCon tt
 = case tt of
	TApp{}
	 | Just (t1, t2, eff, clo)	<- takeTFun tt
	 -> slurpConRegionsCon t1
	 ++ slurpConRegionsCon t2

	 | Just (v, k, ts)		<- takeTData tt
	 -> catMap slurpConRegionsCon ts

	TVar k v		
		| k == kRegion	-> [v]

	TVar _ _		-> []	


