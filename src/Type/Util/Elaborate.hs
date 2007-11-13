-- Handles elaboration of types from the ffi.
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
import Shared.Error 

import Type.Exp
import Type.Pretty
import Type.Util.Bits		
import Type.Plate.Collect

import Debug.Trace

-----
stage	= "Type.Elaborate"


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
 = do	(tRegions, vksRegions)	<- elaborateRegionsT t
 	tClosure		<- elaborateCloT tRegions

	tEffect			<- elaborateEffT (map fst vksRegions) tClosure
	
 	return	$ addTForallVKs vksRegions
		$ tEffect


-----------------------
-- elaborateRegionsT
--	Check the kinds of data type constructors, and if they don't have enough region 
--	args then add new ones to make them the right kind.
--
elaborateRegionsT
	:: Monad m
	=> (?newVarN :: NameSpace -> m Var)	-- ^ fn to create new vars
	-> (?getKind :: Var -> m Kind)		-- ^ fn to get kind of type ctor
	-> Type 				-- ^ the type to elaborate
	-> m ( Type		-- new type
	     , [(Var, Kind)])	-- extra regions

elaborateRegionsT tt
 = do	(tt', vks, fs)	<- elaborateRegionsT' tt
	return	( addFetters fs tt'
		, vks)

elaborateRegionsT' tt

	-- if we see a forall then drop new regions on that quantifier
 	| TForall vks x		<- tt
	= do	(x', vks', fs)	<- elaborateRegionsT' x
		return	( addTForallVKs vks' (TForall vks x')
			, []
			, fs)

	| TFetters fs x		<- tt
	= do	(x', vks, fs')	<- elaborateRegionsT' x
		return	( TFetters (fs ++ fs') x'
			, vks
			, [])

	| TVar{}		<- tt
	=	return 	( tt, [], [])

	| TFun t1 t2 eff clo	<- tt
	= do
		(t1', vks1, fs1)	<- elaborateRegionsT' t1
		(t2', vks2, fs2)	<- elaborateRegionsT' t2
		return	( TFun t1' t2' eff clo
			, vks1 ++ vks2 
			, fs1 ++ fs2)

	-- assume every region under a mutable operator is mutable
	| TMutable x		<- tt
	= do	(x', vks, fs1)	<- elaborateRegionsT' x

		-- collect up all the vars in the rewritten type and choose all the regions
		let vsRegions	= filter (\v -> Var.nameSpace v == NameRegion)
				$ collectVarsT x'

		-- build a Mutable constraint for these regions
		let fNew	= FConstraint primMutable
					[ makeTSum KRegion $ map (TVar KRegion) vsRegions ]

		return	( x'
			, vks
			, fNew : fs1)

	-- add new regions to data type constructors to bring them up
	--	to the right kind.
	| TData v ts		<- tt
	= do	kind		<- ?getKind v
		(ts', vks')	<- elabRs ts kind
		
		return	( TData v ts'
			, vks'
			, [])


-- | Take some arguments from a type ctor and if needed insert fresh region vars
--	so the reconstructed ctor with these args will have this kind.
--
elabRs 	:: Monad m
	=> (?newVarN :: NameSpace -> m Var)	-- ^ fn to create new vars
	-> (?getKind :: Var -> m Kind)		-- ^ fn to get kind of type ctor
	-> [Type]				-- ^ ctor args
	-> Kind					-- ^ kind to turn the ctor into
	-> m ( [Type]		-- new ctor args
	     , [(Var, Kind)])	-- added vars

elabRs args kind
 = do	(args', vks)	<- elabRs' args kind
 	return	( args', nub vks)


elabRs' [] KData
	= return ([], [])

elabRs' [] (KFun k1 k2)
	| KRegion		<- k1
	= do	(ts', vks')	<- elabRs' [] k2
		vR		<- ?newVarN NameRegion
		return		( TVar KRegion vR : ts'
				, (vR, KRegion) : vks')

elabRs' (t:ts) (KFun k1 k2)

	| KRegion		<- k1
	, Just KRegion		<- takeKindOfType t
	= do	(ts', vks')	<- elabRs' ts k2
		return		( t : ts'
				, vks')

	| KRegion		<- k1
	= do	(ts', vks')	<- elabRs' ts k2
		vR		<- ?newVarN NameRegion
		return		( TVar KRegion vR : ts'
				, (vR, KRegion) : vks')

	| otherwise
	= do	(ts', vks')	<- elabRs' ts k2
		return		( t : ts'
				, vks')


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

		let fNew	=
		     case t2 of
			-- rhs of this function is another function
			--	set this closure to be closure of rhs without the arg bound here
			TFun{}	-> FLet cloVarC
				$ TMask KClosure
					(makeTSum KClosure [clo, fromMaybe (TBot KClosure) mClo])
					(TVar KClosure varVal) 

			-- rhs of function isn't another function
			--	pretend that all the args are referenced here.
			_	-> FLet cloVarC (makeTSum KClosure env)

		return	( TFun t1 t2' eff cloVarC
			, fNew : fs
			, Just cloVarC)


-- | Elaborate effects in this type.
elaborateEffT 
	:: Monad m
	=> (?newVarN :: NameSpace -> m Var)
	-> [Var]		-- ^ region variables which were added during elaboration.
	-> Type			-- ^ the type to elaborate
	-> m Type
	
elaborateEffT vsRegion tt
 = do	
 	-- make a fresh hook var.
 	freshHookVar	<- ?newVarN NameEffect

	-- see if there is already a var on the rightmost function arrow, if there isn't one
	--	then add the freshHookVar.
 	let Just (tHooked, hookVar) =
		hookEffT freshHookVar tt
  
    	-- assume that regions added into a contra-variant branch during elaboration 
	--	will be read by the function.
	let rsContra	= slurpConRegions tHooked
	let effsRead	= [ TEffect primRead [TVar KRegion v] 
				| v <- vsRegion 
				, elem v rsContra ]
	
	let tFinal	= addEffectsToFsT effsRead hookVar tHooked
  
 	return tFinal


-- | Find the right most function arrow in this function type and return the effect variable
--	on it, or add this hookVar if there isn't one.
--
hookEffT  :: Var -> Type -> Maybe (Type, Var)
hookEffT hookVar tt
	| TForall vks t		<- tt
	, Just (t', var)	<- hookEffT hookVar t
	= Just	( TForall vks t'
		, var)
	
	| TFetters fs t		<- tt
	, Just (t', var)	<- hookEffT hookVar t
	= Just	( TFetters fs t'
		, var)

 	| TFun t1 t2 eff clo	<- tt
	, TFun{}		<- t2
	, Just (t2', var)	<- hookEffT hookVar t2
	= Just 	( TFun t1 t2' eff clo
	  	, var)
	
	| TFun t1 t2 (TVar KEffect var) clo	<- tt
	= Just	( tt
		, var)
	
	| TFun t1 t2 (TBot KEffect) clo		<- tt
	= Just	( TFun t1 t2 (TVar KEffect hookVar) clo
	  	, hookVar)
	
	| otherwise
	= Nothing


-- | Add some effects to the fetter with this var.
--
addEffectsToFsT
	:: [Effect] -> Var -> Type -> Type
	
addEffectsToFsT effs var tt
 = case tt of
 	TForall vks t1 
	 -> TForall vks (addEffectsToFsT effs var t1)

	TFetters fs t1
	 -> TFetters (FLet (TVar KEffect var) (TSum KEffect effs) : fs) t1
	 
	tx
	 -> TFetters [FLet (TVar KEffect var) (TSum KEffect effs)] tx
	 


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
	

