{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Elaboration of types.
-- 
--   When importing foreign functions, adding all the region effects and closures 
--	annotations to a type by hand is boring and error prone. However, for most 
--	functions this information is fairly unsurprising, so we can fill in most of
--	it automatically.
module DDC.Type.Operators.Elaborate
	( elaborateRsT
	, elaborateRsT_quant
	, elaborateEffT
	, elaborateCloT )
where
import Util
import DDC.Main.Error
import DDC.Type.Exp
import DDC.Type.Builtin
import DDC.Type.Compounds
import DDC.Type.Kind
import DDC.Type.Operators.Pack
import DDC.Type.Operators.Trim
import DDC.Var
import qualified Data.Set		as Set
import qualified Data.Map		as Map

stage	= "Type.Util.Elaborate"

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
	let bks		= [(BVar v, k) | (v, k) <- vks]
	let t_quant	= makeTForall_back bks t_elab

	return t_quant

elaborateRsT newVar tt
 = do	let ?newVar	= newVar
	elaborateRsT' tt

elaborateRsT' tt
 = case tt of
	TVar{}	  -> return (tt, [])

	TSum _ [] -> return (tt, [])

	TForall _ _ x	
	 -> do	(_, vks')	<- elaborateRsT' x
		return	( tt
			, vks')

	TConstrain x crs
	 -> do	(x', vks')	<- elaborateRsT' x
		return	( TConstrain x' crs
			, vks')

	TCon{}	
	 -- some data constructor that isn't applied to any region vars.
   	 | Just (v, kind, ts)	<- takeTData tt
 	 -> elaborateRsT_data tt (v, kind, ts)

 	 -- a function constructor don't need regions added.
	 | otherwise
	 -> return (tt, [])

	TApp t1 t2 
	 | TCon (TyConElaborate{}) <- t1
	 -> do	(t2', vks)	<- elaborateRsT' t2
		return	( TApp t1 t2'
			, vks)

	 -- add new regions to data type constructors to bring them up
	 --	to the right kind.
	 | Just (v, kind, ts)	<- takeTData tt
	 -> elaborateRsT_data tt (v, kind, ts)	
	
	 | otherwise
	 -> do	(t1', vks1)	<- elaborateRsT' t1
		(t2', vks2)	<- elaborateRsT' t2
		return	( TApp t1' t2'
			, vks1 ++ vks2)

	_ -> panic stage
		$ "elaborateRsT': no match for " % tt % "\n\n"
		% " tt = " % show tt % "\n"


elaborateRsT_data _ (v, kind, ts)
 = do	(ts2, vks2)	<- elabRs ts kind
	(ts3, vks3)	<- liftM unzip $ mapM elaborateRsT' ts2

	return	( makeTData v kind ts3
		, vks2 ++ concat vks3)


-- | Take some arguments from a type ctor and if needed insert fresh region vars
--	so the reconstructed ctor with these args will have this kind.
elabRs 	:: (Monad m
	 , ?newVar  :: NameSpace -> m Var)	-- fn to create new region vars
	=> [Type]				-- ^ ctor args
	-> Kind					-- ^ kind to turn the ctor into
	-> m ( [Type]		-- new ctor args
	     , [(Var, Kind)])	-- added vars

elabRs args kind
  = do	(args', vks)	<- elabRs2 args kind
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
		return	( TVar k1 (UVar vR) : ts'
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
					(TVar k1 (UVar vR) : t : ts) 
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


hasKind k tt
 = case tt of
 	TVar k2 _	-> k == k2
	TSum k2 _	-> k == k2
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
	=> (NameSpace -> m Var) 		-- ^ function to use to allocate fresh vars.
	-> Type					-- ^ the type to elaborate
	-> m Type				-- elaborated type

elaborateCloT newVarN tt
 = do	(tt', fs, _)	<- elaborateCloT' newVarN [] tt
  	return	$ pushConstraintsEq (Map.fromList fs) tt'
	
elaborateCloT' newVarN env tt
 = case tt of
	TForall b k x
	 -> do	(x', fs, _)	<- elaborateCloT' newVarN env x
		return	( TForall b k x'
			, fs
			, tEmpty)
			
	-- if we see an existing set of constraints
	--	then drop the new ones in the same place.
	TConstrain x crs
	 -> do	(x', fs', mClo)	<- elaborateCloT' newVarN env x
		let crs'	= Constraints 
					(Map.union (crsEq crs) (Map.fromList fs')) 
					(crsMore crs) (crsOther crs)

		return	( makeTConstrain x' crs'
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

	TApp{}
	 | Just (t1, t2, eff, _)	<- takeTFun tt
	 -> do	-- create a new value variable as a name for the function parameter
		varVal			<- newVarN NameValue
		
		-- elaborate the right hand arg, 
		--	carrying the new parameter name down into it.
		let Just argClo		= liftM trimClosureC
					$ makeTFree varVal t1

		(t2', fs, cloT2)	<- elaborateCloT' newVarN (env ++ [argClo]) t2

		-- make a new closure var to name the closure of this function
		varC			<- newVarN NameClosure
		let cloVarC		= TVar kClosure (UVar varC)

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
		let fNewClo	= (cloVarC, newClo)
		
		-- don't annotate the fn with a closure variable if the closure
		--	in the constraint is just Bot.
		let (fNew, cloAnnot)
			= case fNewClo of 
				(_ , TSum k [])	-> (Nothing, TSum k [])
				_		-> (Just fNewClo, cloVarC)

		return	( makeTFun t1 t2' eff cloAnnot
			, maybeToList fNew ++ fs
			, newClo)

	 | otherwise
	 -> return 	( tt
			, []
			, tEmpty)

	_ -> panic stage $ "elaborateCloT: no match for " % tt



-- Elaborate Effects ------------------------------------------------------------------------------
-- | Elaborate effects in this type.
elaborateEffT 
	:: Monad m
	=> (NameSpace -> m Var)	-- ^ function to allocate fresh variables
	-> [Var]		-- ^ constant region variables to make effects for
	-> [Var]		-- ^ mutable  region variables to make effects for
	-> Type			-- ^ the type to elaborate
	-> m Type		--   the elaborated type
	
elaborateEffT newVarN vsRsConst vsRsMutable tt
 = do	
 	-- make a fresh hook var
	--	The new effect fetter constrains this var.
 	freshHookVar	<- newVarN NameEffect

	-- see if there is already a var on the rightmost function arrow,
	--	if there isn't one then add the freshHookVar.
 	let Just (tHooked, hookVar, hookEffs) =
		hookEffT freshHookVar tt
  
    	-- assume that regions added into a contra-variant
	--	branch during elaboration will be read by the function.
	let rsContra	= slurpConRegions tHooked
	let effsRead	= [ TApp tRead (TVar kRegion $ UVar v)
				| v <- (vsRsConst ++ vsRsMutable)
				, elem v rsContra ]

	let effsWrite	= [ TApp tWrite (TVar kRegion $ UVar v)
				| v <- vsRsMutable
				, elem v rsContra ]
	
	let effs	= effsRead ++ effsWrite ++ maybeToList hookEffs
	
	let tFinal	= addEffectsToFsT effs hookVar tHooked

	-- pack the type to drop out any left-over  !e1 = !Bot  constraints.
  	let tPacked_fast	= packType tFinal

	return $ tPacked_fast
		

-- | Find the right most function arrow in this function type and return the effect variable
--   on it, or add this hookVar if there isn't one.
--
--   If the right most function arrow has a manifest effect on it, then change it to
--   a var and return the original effect.
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
	| TConstrain t crs	<- tt
	, Just (t', var, mEff)	<- hookEffT hookVar t
	= Just	( TConstrain t' crs
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
	| Just (_, _, (TVar _ (UVar var)), _)
					<- takeTFun tt
	= Just	( tt
		, var
		, Nothing )
	
	-- The right-most function has no effects on it.
	--	Add the hook var that we were given.
	| Just (t1, t2, TSum k [], clo)	<- takeTFun tt
	= Just	( makeTFun t1 t2 (TVar k $ UVar hookVar) clo
	  	, hookVar
		, Nothing )
	
	-- The right-most function has some other effect on it. 
	--	Replace this with our hook var and return the effect.
	| Just (t1, t2, eff, clo)		<- takeTFun tt
	= Just	( makeTFun t1 t2 (TVar kEffect $ UVar hookVar) clo
		, hookVar
		, Just eff)
	
	| otherwise
	= freakout stage
		("hookEffT: can't hook type (" % tt % ")")
		Nothing


-- | Add some effects to the fetter with this var.
addEffectsToFsT
	:: [Effect] -> Var -> Type -> Type
	
addEffectsToFsT effs var tt
 = case tt of
 	TForall b k t1 
	 -> TForall b k (addEffectsToFsT effs var t1)

	TConstrain t1 crs
	 -> TConstrain t1 (addEffectsToCrs var effs crs)
	 
	tx
	 -> TConstrain tx 
		(Constraints 
			(Map.singleton (TVar kEffect $ UVar var) (makeTSum kEffect effs))
			Map.empty
			[])
	 

addEffectsToCrs v1 effs crs
 = let	t1	= TVar kEffect (UVar v1) 
   in   case Map.lookup t1 (crsEq crs) of
	 Just eff'
	  -> Constraints
		(Map.insert t1 (makeTSum kEffect (eff' : effs)) $ crsEq crs)
		(crsMore crs)
		(crsOther crs)
		
	 Nothing
	  -> Constraints
		(Map.singleton t1 $ makeTSum kEffect effs)
		Map.empty
		[]
		

-- | Slurp out region variables which appear in contra-variant branches of this type.
slurpConRegions :: Type -> [Var]

slurpConRegions 
 = slurpConRegionsCo

slurpConRegionsCo tt
 = case tt of
 	TForall _ _ t		-> slurpConRegionsCo t
	TConstrain t _		-> slurpConRegionsCo t

	TApp{}
	 | Just (t1, t2, _, _)		<- takeTFun tt
	 -> slurpConRegionsCon t1
	 ++ slurpConRegionsCo  t2
	
	 | Just (_, _, ts)		<- takeTData tt
	 -> catMap slurpConRegionsCo ts

	TVar _ _		-> []
	_			-> panic stage $ "slurpConRegion: no match for " % tt

	 
slurpConRegionsCon tt
 = case tt of
	TApp{}
	 | Just (t1, t2, _, _)		<- takeTFun tt
	 -> slurpConRegionsCon t1
	 ++ slurpConRegionsCon t2

	 | Just (_, _, ts)		<- takeTData tt
	 -> catMap slurpConRegionsCon ts

	TVar k (UVar v)
		| k == kRegion	-> [v]

	TVar _ _		-> []	
	_			-> panic stage $ "slurpConRegionsCon: no match for " % tt


