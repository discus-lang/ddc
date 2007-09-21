
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


elaborateT 
	:: Monad m
	=> (?newVarN 	:: NameSpace 	-> m Var)
	-> (?getKind	:: Var		-> m Kind)
	-> Type -> m Type

elaborateT t
 = do	(tRegions, vks)	<- elaborateRegionsT t
 	tClosure	<- elaborateCloT tRegions

 	return	$ addTForallVKs vks
		$ tClosure


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


{-
elaborateT t
 = do	(t', newEffs)	<- elaborateT2 t	

	let newRs	= map (\(TEffect _ [TVar r]) -> r) newEffs
	(tEffect,  newEs)	<- elaborateEffsT t'
	(tClosure, newCs)	<- pushActiveCloT  [] tEffect

	let moreVks	=  nub
			$  map (\v 	 -> (v, KRegion))  newRs
			++ map (\v  	 -> (v, KEffect))  newEs
			++ map (\(v, cs) -> (v, KClosure)) newCs

	let moreFs	
		=  [ FEffect  (EVar v) (ESum newEffs)	
			|  v 	<- newEs ]

		++ [ FClosure (CVar v) clo		
			| (v, clo) <- newCs ]

		++ [ FClass   primMutable r
			| ECon name r <- newEffs
			, name == primWrite ]

	let tFF		= addForall  moreVks
			$ addFetters moreFs
			$ tClosure
			
	let tMasked	= maskEsFreshT tFF
	

	return		tMasked
			
			
				 

addForall vksMore t
 = case t of
 	TForall vks x	-> TForall (vks ++ vksMore) 	x
	_		-> TForall vksMore		t

addFetters fsMore t
 = case t of
 	TForall  vks x	-> TForall vks (addFetters fsMore x)
	TFetters fs  x	-> TFetters (fs ++ fsMore) x
	_		-> TFetters fsMore t


	

elaborateT2 tt
 = case tt of
	TVar v
	 ->	return (tt, [])
	 
	TFun t1 t2 eff clo
	 -> do	(t1', eff1)	<- elaborateT2 t1
	 	(t2', eff2)	<- elaborateT2 t2
		return	( TFun t1' t2' eff clo
			, eff1 ++ eff2)
		
 	TCon v ts	
	 -> do	k			<- ?getKind v
	 	let kParts		= flattenKind k
		(tsR, newRsHere)	<- elaborateRs ts kParts
		let newEffsHere		= map (\r -> ECon primRead [TRegion (RVar r)]) newRsHere

		(ts', newEffss)		<- liftM unzip
					$  mapM elaborateT2 tsR
		return 	( TCon v ts'
			, newEffsHere ++ concat newEffss)
	 
	TForall vks t
	 -> do	(t', effs)	<- elaborateT2 t
	 	return	( TForall vks t'
			, effs)
	
	TFetters fs t
	 -> do	(t', effs)	<- elaborateT2 t
	 	return	( TFetters fs t'
			, effs)
	
	TRegion{}	-> return (tt, [])
	TEffect{}	-> return (tt, [])
	TClosure{}	-> return (tt, [])
	 	 
	TMutable t
	 -> do	(t', effs)	<- elaborateT2 t
		let effs'
			= map (\e -> case e of
				ECon name r
				 | name == primRead	-> ECon primWrite r
				 | otherwise		-> e)
			$ effs
		
	 	return	( t'
			, effs')

-}



	

{-
-----------------------
-- elaborateEffsT
--	Make sure there are effect vars on active function ctors, 
--	creating them if need be.
--
elaborateEffsT 
	:: Monad m
	=> (?newVarN :: NameSpace -> m Var)	-- ^ the fn to create a new var
	-> Type 				-- ^ type to elaborate
	-> m ( Type				-- elaborated type
	     , [Var])				-- created effect vars
	
elaborateEffsT tt
 	| TForall vks x		<- tt
	= do	(x', vs)	<- elaborateEffsT x
	 	return	( TForall vks x'
			, vs)
		
	| TFetters fs x		<- tt
	= do	(x', vs)	<- elaborateEffsT x
	 	return	( TFetters fs x'
			, vs)

	| TVar{}		<- tt
	= 	return	( tt, [])

	| TCon{}		<- tt
	=	return	( tt, [])
			
	| TFun t1 t2 eff clo	<- tt
	, TFun{}		<- t2
	= do	(t2', vs)	<- elaborateEffsT t2
		return	( TFun t1 t2' eff clo
			, vs)
		
	| TFun t1 t2 eff clo	<- tt
	, EVar v		<- eff
	= 	return	( TFun t1 t2 eff clo
			, [v])
	
	| TFun t1 t2 eff clo	<- tt
	, ENil			<- eff
	= do	v	<- ?newVarN NameEffect
		return	( TFun t1 t2 (EVar v) clo
			, [v])

-}
