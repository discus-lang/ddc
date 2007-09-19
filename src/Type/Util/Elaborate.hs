
module Type.Util.Elaborate
	( elaborateT )

where

-----
import Util

-----
import qualified Shared.Var as Var
import Shared.Var 	(Var, NameSpace(..))
import Shared.VarPrim
import Shared.Error 

import Type.Exp
import Type.Util.Bits		
-- import Type.Effect.MaskFresh	(maskEsFreshT)

-----
stage	= "Type.Elaborate"


elaborateT 
	:: Monad m
	=> (?newVarN 	:: NameSpace 	-> m Var)
	-> (?getKind	:: Var		-> m Kind)
	-> Type -> m Type

elaborateT t
 = do	tClo		<- elaborateCloT t
 	return	tClo


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


elaborateRs tt kk

	| []			<- tt
	, []			<- kk
	= return ([], [])

	| []			<- tt
	, [KType]		<- kk
	= return ([], [])
 
	| t@(TRegion _) : ts	<- tt
	, KRegion   	: ks	<- kk
	= do	(rest, newRs)	<- elaborateRs ts ks
		return	( t : rest
			, newRs)

	| t@(TEffect _) : ts	<- tt
	, KEffect 	: ks	<- kk
	= do	(rest, newRs)	<- elaborateRs ts ks
		return	( t : rest
			, newRs)
		
	| t@(TClosure _) : ts	<- tt
	, KClosure	: ks	<- kk
	= do	(rest, newRs)	<- elaborateRs ts ks
		return	( t : rest
			, newRs)

	| _			<- tt
	, KRegion : ks		<- kk
	= do	r		<- ?newVarN NameRegion 
	 	(rest, newRs)	<- elaborateRs tt ks
		return	( TRegion (RVar r) : rest
			, r : newRs)
		
	| t : ts		<- tt
	, KType		: ks	<- kk
	= do	(rest, newRs)	<- elaborateRs ts ks
		return	( t : rest
			, newRs)
	
	| otherwise
	= panic stage 
	$ "elaborateRs: no match for " % show (tt, kk) % "\n"

-}
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

	-- if this fn ctor has no closure then assume it references
	--	everything passed into the outer function.
	| TFun t1 t2 eff clo	<- tt
	= do	
		-- create a new var to label the arg
		varVal		<- ?newVarN NameValue
		
		-- elaborate the right hand carrying the new argument down into it
		let argClo	= TFree varVal t1
		(t2', fs, mClo)	<- elaborateCloT' (env ++ [argClo]) t2

		-- the closure for this function is the body minus the var for the arg
		varC		<- ?newVarN NameClosure
		let cloVarC		= TVar KClosure varC

		let thisClo	= TMask KClosure
					(makeTSum KClosure [clo, fromMaybe (TBot KClosure) mClo])
					(TVar KClosure varVal) 

		let f1		= case t2 of
					TFun{}	-> FLet cloVarC thisClo
					_	-> FLet cloVarC (makeTSum KClosure env)

		return	( TFun t1 t2' eff cloVarC
			, f1 : fs
			, Just cloVarC)

	

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
